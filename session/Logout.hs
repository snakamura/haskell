{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main where

import Network.FastCGI (runFastCGIorCGI)
import Network.NewCGI

import Session


main :: IO ()
main = runFastCGIorCGI $ handleErrors $ process

process :: CGI CGIResult
process = do logout
             redirect loginPageURI
