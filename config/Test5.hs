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
loadConfig = return $ Config "Test5_.hs"


type ConfigReaderT = ReaderT Config
type ConfigReader = ConfigReaderT IO

instance MonadError Exception ConfigReader where
    throwError = liftIO . throwIO
    catchError c h = tryCR c >>= either h return

tryCR :: ConfigReader a -> ConfigReader (Either Exception a)
tryCR cr = ReaderT $ \r -> try (runReaderT cr r)

main :: IO ()
main = do config <- loadConfig
          runReaderT echo config `catchDyn` (\e -> putStrLn $ "Error!!! " ++ e)

echo :: ConfigReader ()
echo = do name <- asks configFile
          file <- liftIO $ B.readFile name
          liftIO $ B.hPut stdout file
       `catchError` \_ -> throwError $ DynException $ toDyn "Exception occured."
