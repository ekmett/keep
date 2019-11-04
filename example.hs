{-# Language
  OverloadedStrings, DeriveAnyClass, DeriveGeneric,
  DerivingStrategies, LambdaCase #-}
import Control.Keep
import Control.Monad.IO.Class
import Data.Binary as Binary
import Data.Hashable
import Database.Redis
import GHC.Generics

data Tree = Bin Int (Merkle Tree) (Merkle Tree) | Tip
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, Hashable)

-- mainClosure :: Closure (Keep Int)
--
instance MonadIO Keep where
  liftIO = unsafeRedis . liftIO

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ runKeep "example" "Example context" $ do
    x <- Bin 4 <$> merkle Tip <*> merkle Tip
    liftIO $ print x
    case Binary.decode (Binary.encode x) of
      Bin 4 y z -> do
        y' <- unmerkle y
        liftIO $ print y'
      Tip -> fail "bad"
    return ()
