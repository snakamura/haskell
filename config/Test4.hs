module Main (main) where

import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import System.IO

data Config = Config {
    configFile :: String
}

loadConfig :: IO Config
loadConfig = return $ Config "Test4.hs"


type ConfigReaderT = ReaderT Config
type ConfigReader = ConfigReaderT IO


main :: IO ()
main = do config <- loadConfig
          runReaderT echo config

echo :: ConfigReader ()
echo = do
    name <- asks configFile
    file <- liftIO $ B.readFile name
    liftIO $ B.hPut stdout file
