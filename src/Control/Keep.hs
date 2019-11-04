{-# Language BlockArguments #-}
{-# Language TupleSections #-}
{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

module Control.Keep
  ( Auth, auth, unauth
  , Keep, runKeep
  , closed
  , cached
  , distrust
  ) where

import Control.Distributed.Closure
import Control.Exception (catch)
import Control.Lens
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Crypto.Hash.SHA256
import Data.Binary
import Data.ByteString as Strict
import Data.ByteString.UTF8 as UTF8
import Data.Time
import Data.UUID
import Data.UUID.V4 as V4
import Database.Redis
import Database.Redis.Core.Internal

-- redis state:

-- keep.log: list -- contains log of bad traces
-- "$hash" : string -- merkled data
-- "t:$uuid" : list -- in-progress execution traces, first entry is hash of closure id
-- "c:$hash" : list -- final execution trace of a closure, first entry is hash of closure id

data Mode
  = Play {-# unpack #-} !(IORef [ByteString]) Bool
  | Record ByteString Bool
  | Stop Bool

trust :: Lens' Mode Bool
trust f (Stop b) = Stop <$> f b
trust f (Record uuid b) = Record uuid <$> f b
trust f (Play r b) = Play r <$> f b

newtype Keep a = Keep (ReaderT Mode Redis a) deriving (Functor,Applicative,Monad)

unKeep :: Keep a -> Mode -> Redis a
unKeep (Keep m) = runReaderT m

-- locally disable cache trust
distrust :: Keep a -> Keep a
distrust (Keep m) = Keep \e -> m (e & trust .~ False)

hashEncode :: Binary a => a -> ByteString
hashEncode = hashlazy . encode

data BadTrace = BadTrace String deriving (Show, Exception)

freshTrace :: MonadIO m => m ByteString
freshTrace = liftIO do
  uuid <- V4.next
  pure $ "t:" <> UUID.toAsciiBytes uuid

record :: ByteString -> Bool -> Keep a -> Redis a
record h t m = do
  k <- freshTrace
  lpush k h -- allows us to identify the trace
  unKeep m $ Record k t
  rename k h
  lset k 0 "" -- might as well cleanup

warn :: HasCallStack => String -> Redis ()
warn s = do
  t <- liftIO getCurrentTime
  let cs = drop 1 $ lines $ prettyCallStack $ popCallStack callStack)
  let s' = intersperse '\n' $ (show t ++ " warning: "  ++ s) : cs
  liftIO $ Prelude.putStrLn s'
  rpush "keep.log" $ UTF8.pack s'

withRunRedisInIO :: ((forall a. Redis a -> IO a) -> IO b) -> Redis b
withRunRedisInIO inner = reRedis $ withRunInIO $ \run -> inner (run . unRedis)

closed :: HasCallStack => Closure (Keep a) -> Keep a
closed c = Keep \e -> do
  let h = "c:" <> hashEncode c
  let m = unclosure c
  trace <- lrange h 0 -1
  if null trace then record h (e^.trust) m
  else do
    r <- newIORef (tail trace)
    withRunRedisInIO \run ->
        run (unKeep k $ Play r (e^.trust))
      `catch` \ (BadTrace msg) -> run do
        del h
        warn $ msg <> " (running " <> show h <> ")"
        record h k e

runKeep :: Keep a -> Redis a
runKeep (Keep m) = m (Stop True)

auth :: Binary a => a -> Keep (Auth a)
auth a = Keep \case
  Play{} -> pure $ Verified h -- Proof a h? why not work locally when we can
  _ -> Proof a h <$ setnx h v
 where
   v = toStrict $ encode a
   h = hash v

-- TODO: compile into a lua script we can eval redis-side to remove a round trip
getAndRPush :: ByteString -> ByteString -> Redis a
getAndRPush t h = do
  v <- get h
  rpush t v

unauth :: Binary a => Auth a -> Keep a
unauth (Verified h) = Keep \case
  Play r trusting -> do
    v <- liftIO $ join $ atomicModifyIORef r $ \case
      v:vs -> (vs, pure v)
      [] -> (vs, throwIO $ BadTrace "unauth")
    unless (trusting || hash r == h) throw $ BadTrace $ "hash mismatch " <> show h <> " /= " show hash r
    case decodeOrFail r of
      Left (_,_,msg) -> throw (BadTrace msg)
      Right v  -> pure v
  Record t _ -> decode <$> getAndRPush t h
  Stop _ -> get h
unauth (Proof a h) = Keep \case
  Stop _     -> pure a
  Record t _ -> a <$ rpush t a
  Play r trusting -> do
    v <- liftIO $ join $ atomicModifyIORef r $ \case
      v:vs -> (vs, pure v)
      [] -> (vs, throwIO $ BadTrace "unauth")
    if trusting then pure a
    else do
      unless (trusting || hash r == h) throw $ BadTrace $ "hash mismatch " <> show h <> " /= " show hash r
      case decodeOrFail r of
        Left (_,_,msg) -> throwIO $ BadTrace msg
        Right v  -> pure v

-- this might be better if i just had verified and had to round trip to redis or if the 'a' was a promise?
data Auth a
  = Proof a Hash
  | Verified Hash

instance Binary Auth where
  put (Proof _ h) = put h
  put (Verified h) = put h
  get = Verified <$> get

-- run a subcomputation, stashing the answer in the trace in a trustable form
-- but if you don't trust, you can still verify the subtrace
cached :: Binary a => Keep a -> Keep a
cached m = Keep \case
  Play r trusting -> do
    (t',v) <- join $ atomicModifyIORef r $ \case
      t':v:xs -> (xs, pure (t',v))
      _ -> (xs, throwIO $ BadTrace "cached")
    if trusting
    then pure $ decode v
    else do
      t' -- trace subcomputation t'

  Record t trusting -> do
    t' <- freshTrace
    i <- llen t
    lpush t' $ pack $ "cached:" <> unpack t <> (':':show i) -- show where it came from
    a <- unKeep m $ Record t' trusting
    rpush t t'
    a <$ rpush t (encode a) -- result

  s@Stop{} -> unKeep m s

  -- we have some keep computation we'd like to run
  -- so we'll make up a name for where we are in record, and use it in playback

  k <- freshTrace
  lpush k h -- allows us to identify the trace
