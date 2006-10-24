{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Main (main) where

import Control.Exception (Exception(..), catchDyn, throwIO, try)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Dynamic (toDyn)
import System.IO

data Config = Config {
    configValue :: Int
}

loadConfig :: IO Config
loadConfig = return $ Config 8


newtype ConfigReaderT m a = ConfigReaderT { unCR :: ReaderT Config m a }
type ConfigReader = ConfigReaderT IO

instance Monad m => Monad (ConfigReaderT m) where
    cr >>= f = ConfigReaderT $ unCR cr >>= unCR . f
    return = ConfigReaderT . return

instance MonadTrans ConfigReaderT where
    lift = ConfigReaderT . lift

instance MonadIO m => MonadIO (ConfigReaderT m) where
    liftIO = lift . liftIO

instance Monad m => MonadReader Config (ConfigReaderT m) where
    ask = ConfigReaderT $ ask
    local f c = ConfigReaderT $ ReaderT $ runReaderT (unCR c) . f

main :: IO ()
main = do config <- loadConfig
          s <- runReaderT (unCR (execStateT add 5)) config
          print s

add :: StateT Int (ConfigReaderT IO) ()
add = do n <- lift $ asks configValue
         modify (+n)
         liftIO $ print n
{-
add = local (\ (Config x) -> Config (x + 1)) $ do
   n <- lift $ asks configValue
   modify (+n)
   liftIO $ print n
-}
