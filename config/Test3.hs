module Main (main) where

import qualified Data.ByteString.Lazy as B
import System.IO

data Config = Config {
    configFile :: String
}

loadConfig :: IO Config
loadConfig = return $ Config "Test3.hs"


main :: IO ()
main = do config <- loadConfig
          echo config

echo :: Config -> IO ()
echo config = do
    file <- B.readFile $ configFile config
    B.hPut stdout file
    