{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Main (main) where

import Control.Exception (Exception(..), catchDyn, throwIO, try)
import Control.Monad.Error
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Dynamic (toDyn)
import System.IO

data Config = Config {
    configFile :: String
}

loadConfig :: IO Config
loadConfig = return $ Config "Test6_.hs"


newtype ConfigReaderT m a = ConfigReaderT { unCR :: ReaderT Config m a }
type ConfigReader = ConfigReaderT IO

instance Monad m => Monad (ConfigReaderT m) where
    cr >>= f = ConfigReaderT $ unCR cr >>= unCR . f
    return = ConfigReaderT . return

instance MonadTrans ConfigReaderT where
    lift = ConfigReaderT . lift

instance MonadIO ConfigReader where
    liftIO = lift

class Monad m => MonadConfig m where
    getConfig :: (Config -> a) -> m a

instance Monad m => MonadConfig (ConfigReaderT m) where
    getConfig = ConfigReaderT . asks

instance MonadError Exception ConfigReader where
    throwError = liftIO . throwIO
    catchError c h = tryCR c >>= either h return

tryCR :: ConfigReader a -> ConfigReader (Either Exception a)
tryCR cr = ConfigReaderT $ ReaderT $ \r -> try (runReaderT (unCR cr) r)

main :: IO ()
main = do config <- loadConfig
          runReaderT (unCR echo) config `catchDyn` (\e -> putStrLn $ "Error!!! " ++ e)

echo :: ConfigReader ()
echo = do name <- getConfig configFile
          file <- liftIO $ B.readFile name
          liftIO $ B.hPut stdout file
       `catchError` \_ -> throwError $ DynException $ toDyn "Exception occured."
