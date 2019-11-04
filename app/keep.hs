{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
import Control.Keep.Internal
import Control.Monad.IO.Class
import Data.ByteString.UTF8 as UTF8
import Data.Foldable
-- import Data.Traversable
import Database.Redis hiding (info)
import Options.Applicative
import System.Exit

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <.> g = (.) <$> f <*> g

parseConnection :: Parser (IO Connection)
parseConnection = connectWith <$> uri <*> fieldByField where
  uri = parseConnectInfo <$> strOption (long "uri" <> metavar "URI" <> showDefault <> value "redis://localhost:6379/" <> help "redis connect uri")

  fieldByField = host <.> port <.> password -- database <.> maxConnections <.> maxIdleTime <*> connectTimeout <.> tlsParams TODO
  host = by (\a m -> m { connectHost = a }) $ strOption (long "host" <> metavar "HOST" <> help "host")

  port = by (\a m -> m { connectPort = a }) $
        UnixSocket <$> strOption (long "socket" <> metavar "SOCKET" <> help "unix socket")
    <|> PortNumber <$> option auto (long "port" <> showDefault <> help "port")

  password = by (\a m -> m { connectAuth = a }) $ Just <$> strOption (long "password" <> metavar "PASSWORD" <> help "password")
           
  by :: (a -> ConnectInfo -> ConnectInfo) -> Parser a -> Parser (ConnectInfo -> ConnectInfo)
  by f p = maybe id f <$> optional p

  connectWith (Left uriError) _ = fail uriError
  connectWith (Right ci)      p = checkedConnect $ p ci


-- sketch of an interface

-- if i move the redis connection config to a config file, then these can autocomplete in .zshrc
  
-- cabal run keep -- log
-- cabal run keep -- log --last
-- cabal run keep -- log 0 "(-1)"
-- cabal run keep -- echo "hello"
-- cabal run keep -- context list
-- cabal run keep -- context remove <context-id> --dry-run
--
-- cabal run keep -- context show <context-id>
-- cabal run keep -- context rename <old-context-id> <new-context-id>
-- cabal run keep -- trace gc
-- cabal run keep -- trace list
-- cabal run keep -- trace show <trace-id>
-- cabal run keep -- closure list
-- cabal run keep -- closure show <trace-id>
-- cabal run keep -- repl

parseCommand :: Parser (Redis ())
parseCommand = hsubparser (log <> echo <> context) where
  log = command "log" $ info logParser $ progDesc "play back the most recent keep log lines from redis"
  logParser = runLog <$> logStart <*> logEnd
  logStart = argument auto (metavar "LINE" <> value 0) 
  logEnd   = argument auto (metavar "LINE" <> value (-1)) 

  echo = command "echo" $ info echoParser $ progDesc "perform a round-trip test"
  echoParser = runEcho <$> strArgument (metavar "STRING" <> value "") 

  context = command "context" $ info contextParser $ progDesc "context commands"
  contextParser = hsubparser (contextList <> contextRemove {- <> contextShow <> ... -})

  contextList = command "list" $ info contextListParser $ progDesc "list current contexts"
  contextListParser = pure runContextList

  contextRemove = command "remove" $ info contextRemoveParser $ progDesc "remove a context"
  contextRemoveParser = runContextRemove
     <$> strArgument (metavar "CONTEXT")
     <*> switch ( long "dry-run" <> short 'd' <> help "describe targets" )

runLog :: Integer -> Integer -> Redis ()
runLog start stop = do
  xs <- check $ lrange "keep.log" start stop
  liftIO $ for_ xs $ Prelude.putStrLn . reverse . dropWhile (=='\n') . reverse . UTF8.toString 

runEcho :: ByteString -> Redis ()
runEcho msg = do
  xs <- check $ echo msg
  liftIO $ Prelude.putStrLn $ UTF8.toString xs

runContextList :: Redis ()
runContextList = do
  xs <- check $ hgetall "keep.contexts"
  liftIO $ for_ xs $ \(c,desc) -> Prelude.putStrLn $ UTF8.toString c <> " " <> UTF8.toString desc

runContextRemove :: ByteString -> Bool -> Redis ()
runContextRemove c dryRun = do
  -- TODO: check for clients and warn if any are for the current context
  let traces = c <> ".traces"
  let closures = c <> ".closures"
  (xs,ys) <- checkTx $ liftA2 (,) <$> hkeys traces <*> hkeys closures
  if dryRun then liftIO $ for_ (xs ++ ys) $ putStrLn . UTF8.toString
  else do
    () <$ checkTx do
      del $ (traces : xs) ++ (closures : ys)
      hdel "keep.contexts" [c]
    
  
  -- dry run remove all traces, closures
  
parser :: Parser (IO Connection, Redis ())
parser = (,) <$> parseConnection <*> parseCommand

main :: IO ()
main = do
  (connector, command) <- customExecParser (prefs $ showHelpOnEmpty <> subparserInline) $
    info (parser <**> helper) $ 
       fullDesc
    <> progDesc "Keep command line tool"
    <> header "keep - distributed persistent computation"
  connection <- connector
  runRedis connection $ do
    -- i <- check clientId
    -- liftIO $ putStrLn $ "client id: " ++ show i
    command
