{-# Language OverloadedStrings #-}
import Control.Keep
import Database.Redis

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ runKeep "example" "Example context" $ return ()
