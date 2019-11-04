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
  uri = optional $ parseConnectInfo <$> strOption (long "uri" <> metavar "URI" <> help "redis connect uri")

  fieldByField = host <.> port <.> password -- database <.> maxConnections <.> maxIdleTime <*> connectTimeout <.> tlsParams TODO
  host = by (\a m -> m { connectHost = a }) do
    strOption (long "host" <> metavar "HOST" <> value "localhost" <> showDefault <> help "host")

  port = by (\a m -> m { connectPort = a }) $
        UnixSocket <$> strOption (long "socket" <> metavar "SOCKET" <> help "unix socket")
    <|> PortNumber <$> option auto (long "port" <> metavar "PORT" <> value 6379 <> showDefault <> help "port")

  password = by (\a m -> m { connectAuth = a }) $ Just <$> strOption (long "password" <> metavar "PASSWORD" <> help "password")
           
  by :: (a -> ConnectInfo -> ConnectInfo) -> Parser a -> Parser (ConnectInfo -> ConnectInfo)
  by f p = maybe id f <$> optional p

  connectWith (Just (Left uriError)) _ = fail uriError
  connectWith (Just (Right ci))      p = checkedConnect $ p ci
  connectWith Nothing                p = checkedConnect $ p defaultConnectInfo

  
parseCommand = hsubparser (log {- <> other parsers -}) where
  log = command "log" $ info logParser $ progDesc "play back the most recent keep log lines from redis"
  logParser = runLog <$> logStart <*> logEnd
  logStart = argument auto (metavar "LINE" <> value 0) 
  logEnd   = argument auto (metavar "LINE" <> value (-1)) 

runLog :: Integer -> Integer -> Redis ()
runLog start stop = do
  xs <- check $ lrange "keep.log" start stop
  liftIO $ for_ xs $ Prelude.putStrLn . reverse . dropWhile (=='\n') . reverse . UTF8.toString 
  
parser :: Parser (IO Connection, Redis ())
parser = (,) <$> parseConnection <*> parseCommand

main :: IO ()
main = do
  (connector, command) <- execParser $ info (parser <**> helper) $ 
       fullDesc
    <> progDesc "Manage a redis cache for the keep library"
    <> header "keep - distributed persistent computation"
  connection <- connector
  runRedis connection command
