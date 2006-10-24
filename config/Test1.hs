{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Network.NewCGI


main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
    file <- liftIO $ B.readFile "Test1.hs"
    outputFPS file

