{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Network.NewCGI

data Config = Config {
    configFile :: String
}

loadConfig :: IO Config
loadConfig = return $ Config "Test2.hs"


main :: IO ()
main = do config <- loadConfig
          runCGI $ handleErrors $ cgiMain config

cgiMain :: Config -> CGI CGIResult
cgiMain config = do
    file <- liftIO $ B.readFile $ configFile config
    outputFPS file

