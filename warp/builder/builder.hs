{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Enumerator (($$), (=$), catchError, run_, tryIO)
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Network.HTTP.Types (headerContentType, status200, status500)
import Network.Wai (Application, Response(..), responseLBS)
import Network.Wai.Handler.Warp (run)
import System.IO (IOMode(ReadMode), hClose, hSetEncoding, openFile, utf8)


main :: IO ()
main = run 8000 (handleError upper)
  where
    handleError app req = app req `catchError` (const $ return $ responseLBS status500 [headerContentType "text/html"] "<h1>Internal error</h1>")


upper :: Application
upper _ = do
    t <- tryIO $ liftIO $ bracket (openFile "builder.hs" ReadMode) hClose $ \h -> do
           hSetEncoding h utf8
           run_ $ ET.enumHandle h $$ EL.map T.toUpper =$ EL.isolate 10 =$ EL.concatMap (:["\n"]) =$ EL.map fromText =$ EL.consume
    return $ ResponseBuilder status200 [headerContentType "text/plain"] $ mconcat t
