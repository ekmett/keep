{-# Language
  BlockArguments, TupleSections, PatternSynonyms, OverloadedStrings,
  BangPatterns, RankNTypes, LambdaCase, DeriveFunctor, DeriveAnyClass,
  GeneralizedNewtypeDeriving, DerivingStrategies #-}

module Control.Keep.Internal where

import Control.Distributed.Closure
import Control.Exception (Exception(..), catch, throwIO)
import Control.Monad (join, unless)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Crypto.Hash.SHA256 as Crypto
import Data.Binary as Binary
import Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Char8 as Char8
import Data.ByteString.UTF8 as UTF8
import Data.Function (on)
import Data.IORef
import Data.Hashable as Hashable
import Data.List as List
import Data.Time
import Data.Typeable
import Data.UUID as UUID
import Data.UUID.V4 as V4
import Database.Redis as Redis
import Database.Redis.Core.Internal
import GHC.Stack

-- redis state:

-- keep.log: list -- contains log messages
-- "$hash" : string -- merkled data, easily checked for validity
-- "t:$uuid" : list -- in-progress execution traces, first entry is hash of closure id
-- "$context:$hash" : list -- final execution trace of a closure, first entry is hash of closure id

data Mode
  = Play
  { _playbackLog :: {-# unpack #-} !(IORef [ByteString])
  , ctx :: ByteString
  , trusting :: Bool
  }
  | Record
  { _recordingTrace :: ByteString
  , _path :: ByteString -- canonical name for the current trace
  , ctx :: ByteString
  , trusting :: Bool -- local trace id, canonical path hash, trusting?
  }
  | Stop -- trusting?
  { ctx :: ByteString
  , trusting :: Bool
  }

data BadTrace = BadTrace String deriving (Show, Exception)
data RedisError = RedisError Reply deriving (Show, Exception)

newtype Keep a = Keep (ReaderT Mode Redis a)
  deriving stock (Functor)
  deriving newtype (Applicative,Monad)

keep :: (Mode -> Redis a) -> Keep a
keep = Keep . ReaderT

localKeep :: (Mode -> Mode) -> Keep a -> Keep a
localKeep f m = keep \e -> unKeep m (f e)

unKeep :: Keep a -> Mode -> Redis a
unKeep (Keep m) = runReaderT m

-- locally disable cache trust
verify :: Keep a -> Keep a
verify = localKeep $ \m -> m { trusting = False }

hashEncode :: Binary a => a -> Strict.ByteString
hashEncode = hashlazy . Binary.encode

encodeStrict :: Binary a => a -> Strict.ByteString
encodeStrict = Lazy.toStrict . Binary.encode

decodeStrict :: Binary a => Strict.ByteString -> a
decodeStrict = Binary.decode . Lazy.fromStrict

freshTrace :: MonadIO m => m ByteString
freshTrace = liftIO do
  uuid <- V4.nextRandom
  pure $ "t:" <> UUID.toASCIIBytes uuid

throwM :: (MonadIO m, Exception e) => e -> m a
throwM = liftIO . throwIO

check :: Redis (Either Reply b) -> Redis b
check m = do
  m >>= \case
    Left e -> throwM $ RedisError e
    Right a -> pure a

unsafeRedis :: Redis a -> Keep a
unsafeRedis m = keep \_ -> m

warn :: HasCallStack => String -> Redis ()
warn s = do
  t <- liftIO getCurrentTime
  let cs :: [String]
      cs = Prelude.drop 1 $ Prelude.lines $ prettyCallStack $ popCallStack callStack
      s' :: String
      s' = Prelude.unlines $ (show t ++ " warning: "  ++ s) : cs

  liftIO $ Prelude.putStr s'
  () <$ check (rpush "keep.log" [UTF8.fromString s'])

withRunRedisInIO :: ((forall a. Redis a -> IO a) -> IO b) -> Redis b
withRunRedisInIO inner = Redis $ withRunInIO \run -> inner (run . unRedis)

-- TODO:
-- closedCheckpoint :: Binary a => Closure (Keep a) -> Keep a
-- closedCheckpoint = closed . staticMap (static checkpoint)

-- closures are roots
closed :: (HasCallStack, Typeable a) => Closure (Keep a) -> Keep a
closed c = keep \e -> closedPath (ctx e) (ctx e <> ":" <> hashEncode c) (trusting e) (unclosure c)

closedPath :: HasCallStack => ByteString -> ByteString -> Bool -> Keep a -> Redis a
closedPath c p t m = check (lrange p 0 (-1)) >>= \trace -> if Prelude.null trace
  then record
  else do
    withRunRedisInIO \io -> io (play trace)
      `catch` \ (BadTrace msg) -> io do
        _ <- check $ del [p]
        warn $ msg <> " (running " <> show p <> ")" -- errors while playing just cause re-recording for now
        record -- errors while recording get thrown at you
  where
    play trace = do
      r <- liftIO $ newIORef (Prelude.tail trace)
      unKeep m $ Play r c t
    record = do
      k <- freshTrace
      check $ lpush k [p] -- allows us to identify the trace
      a <- unKeep m $ Record k p c t
      check $ rename k p
      a <$ check (lset k 0 "") -- might as well cleanup

next :: MonadIO m => IORef [ByteString] -> m ByteString
next r = liftIO $ join $ atomicModifyIORef r \case
  x:xs -> (xs, pure x)
  xs -> (xs, throwIO $ BadTrace "next")

-- run a subcomputation, stashing the answer in the trace in a trustable form
-- but if you don't trust, you can still verify the subtrace
checkpoint :: Binary a => Keep a -> Keep a
checkpoint m = keep \case
  Play r c trusting -> do
    p <- next r
    v <- next r
    if trusting
    then pure (decodeStrict v)
    else do
      a <- closedPath c p trusting m
      a <$ liftIO do
        unless (Binary.encode a == Lazy.fromStrict v) $ throwIO $ BadTrace "checkpoint result mismatch"
  Record t p c trusting -> do
    i <- check $ llen t -- current computation index
    let p' = c <> ":" <> Crypto.hash (p <> ":" <> Char8.pack (show i))
    check $ rpush t [p']
    a <- closedPath c p' trusting m
    a <$ check (rpush t [encodeStrict a])
  s@Stop{} -> unKeep m s -- can't checkpoint while stopped

-- run a trusted computation on redis
--
-- If you don't trust the result's you'll get back from the database, you can @'runKeep' ctx . 'verify'@
-- and we'll verify the results
--
-- The context should uniquely define the executable StaticPtrs we'll encounter
-- it can be a hash of the executable name, or a hash of the set of dependencies if you share closures
-- across a wider set of executables, etc.
runKeep :: ByteString -> Keep a -> Redis a
runKeep c m = unKeep m (Stop c True)

merkle :: Binary a => a -> Keep (Merkle a)
merkle a = keep \case
  Play{} -> pure $ Local a h
  _      -> Local a h <$ check (setnx h v)
 where
   v = Lazy.toStrict $ encode a
   h = Crypto.hash v

-- TODO: compile into a lua script we can eval redis-side to remove a round trip
getAndRPush :: ByteString -> ByteString -> Redis ByteString
getAndRPush t h = check (Redis.get h) >>= \case
  Nothing -> throwM $ BadTrace $ "store missing key " <> show h
  Just v -> v <$ check (rpush t [v])

unmerkle :: Binary a => Merkle a -> Keep a
unmerkle (Remote h) = keep \case
  Play r _ trusting -> liftIO $ do
    v <- next r
    unless trusting do
      let hv = Crypto.hash v
      throwIO $ BadTrace $ "hash mismatch " <> show h <> " /= " <> show hv
    case decodeOrFail (Lazy.fromStrict v) of
      Left (_,_,msg) -> throwIO $ BadTrace msg
      Right (_,_,a) -> pure a
  Record t _ _ _ -> decodeStrict <$> getAndRPush t h
  Stop _ _ -> check (Redis.get h) >>= \case
    Nothing -> throwM $ BadTrace $ "store missing key " <> show h
    Just v -> pure $ decodeStrict v
unmerkle (Local a h) = keep \case
  Stop _ _ -> pure a
  Record t _ _ _ -> a <$ check (rpush t [encodeStrict a])
  Play r _ trusting -> next r >>= \v -> liftIO $ if trusting
    then pure a
    else do
      let hv = Crypto.hash v
      unless (hv == h) $ throwIO $ BadTrace $ "hash mismatch " <> show h <> " /= " <> show hv
      case decodeOrFail (Lazy.fromStrict v) of
        Left (_,_,msg) -> throwIO $ BadTrace msg
        Right (_,_,a') -> pure a'

data Merkle a
  = Local a ByteString -- local data
  | Remote ByteString

hashMerkle :: Merkle a -> ByteString
hashMerkle (Local _ h) = h
hashMerkle (Remote h) = h

instance Eq (Merkle a) where
  (==) = (==) `on` hashMerkle

instance Hashable (Merkle a) where
  hash = Hashable.hash . hashMerkle
  hashWithSalt salt = Hashable.hashWithSalt salt . hashMerkle

instance Ord (Merkle a) where
  compare = compare `on` hashMerkle

instance Binary (Merkle a) where
  put (Local _ h) = put h
  put (Remote h) = put h
  get = Remote <$> Binary.get
