module Main where

import Control.Monad (liftM)
import Curl
import Data.IORef
import Data.Maybe (fromJust)
import Network.CGI (urlEncode)
import Network.URI
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    let url = fromJust $ parseAbsoluteURI $ args !! 0
    (code, status, url, headers, body) <- httpGet url
    print url

httpGet :: URI -> IO (CurlCode, Int, Maybe URI, [(String, String)], String)
httpGet uri = httpRequest uri options
 where
     options = [CurlNoBody False,
                CurlFollowLocation True,
                CurlMaxRedirs 4,
                CurlAutoReferer True]

httpPost :: URI -> [(String, String)] -> IO (CurlCode, Int, Maybe URI, [(String, String)], String)
httpPost uri values = httpRequest uri options
 where
     options = [CurlPost True,
                CurlNoBody False,
                CurlPostFields $ map encodeValue values]
     encodeValue (n, v) = urlEncode n ++ "=" ++ urlEncode v

httpRequest :: URI -> [CurlOption] -> IO (CurlCode, Int, Maybe URI, [(String, String)], String)
httpRequest uri options = withCurlDo $ do
    headersRef <- newIORef []
    bodyRef <- newIORef []
    h <- initialize
    mapM_ (setopt h) [CurlURL $ show uri,
                      CurlHeaderFunction $ headerFunction headersRef,
                      CurlWriteFunction $ gatherOutput bodyRef]
    mapM_ (setopt h) options
    code <- perform h
    if code == CurlOK
        then do
            status <- getResponseCode h
            IString url <- getInfo h EffectiveUrl
            body <- liftM (concat . reverse) $ readIORef bodyRef
            headers <- liftM reverse $ readIORef headersRef
            return (code, status, parseAbsoluteURI url, headers, body)
        else return (code, 500, Nothing, [], "")



headerFunction :: IORef [(String, String)] -> WriteFunction
headerFunction headersRef = callbackWriter $ \s -> do
    let header = case break (==':') s of
                     (n, ':':v) -> (n, dropWhile (==' ') $ dropNewLine v)
                     (n, _    ) -> (dropNewLine n, "")
    case header of
        ("", "") -> return ()
        (_, "")  -> writeIORef headersRef []
        _        -> modifyIORef headersRef (header:)
 where
     dropNewLine = takeWhile (\c -> c /= '\r' && c /= '\n')

followRedirect :: URI -> IO (Maybe URI)
followRedirect uri = withCurlDo $ do
    h <- initialize
    mapM_ (setopt h) [CurlURL $ show uri,
                      CurlNoBody True,
                      CurlFollowLocation True,
                      CurlMaxRedirs 4,
                      CurlAutoReferer True]
    code <- perform h
    if code == CurlOK
        then do
            IString url <- getInfo h EffectiveUrl
            return $ parseAbsoluteURI url
        else return Nothing
