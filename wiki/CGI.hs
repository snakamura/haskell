module CGI (HTTPRequest(..),
            HTTPResponse(..),
            process,
            Method(..),
            getRequestMethod,
            getQuery,
            Param,
            Params,
            getParam,
            parseParams)
    where

import qualified System.Environment as Env
import qualified System.IO          as IO
import qualified System.IO.Error    as IOError

import qualified HTMLUtil
import qualified TextUtil


data HTTPRequest = HTTPRequest {
                       requestMethod :: Method,
                       requestHeader :: [(String, String)],
                       requestBody   :: String,
                       requestParams :: Params
                   }

data HTTPResponse = HTTPResponse {
                       responseHeader :: [(String, String)],
                       responseBody   :: String
                    }

process :: (HTTPRequest -> IO HTTPResponse) -> IO ()
process proc = request >>= proc >>= printResponse
    where
        request :: IO HTTPRequest
        request = do method <- getRequestMethod
                     -- TODO
                     header <- return []
                     body <- IO.hGetContents IO.stdin
                     params <- case method of
                                    GET  -> getQuery >>= return . parseParams
                                    POST -> return $ parseParams body
                     return $ HTTPRequest method header body params
        printResponse :: HTTPResponse -> IO ()
        printResponse (HTTPResponse header body) =
            do mapM_ (uncurry printHeader) header
               putStr "\r\n"
               putStr body
        printHeader :: String -> String -> IO ()
        printHeader name value = do putStr name
                                    putStr ": "
                                    putStr value
                                    putStr "\r\n"


data Method = GET
            | POST

getRequestMethod :: IO Method
getRequestMethod = Env.getEnv "REQUEST_METHOD" >>= return . getMethod
    where
        getMethod :: String -> Method
        getMethod method | method == "POST" = POST
                         | otherwise        = GET

getQuery :: IO String
getQuery = catch (Env.getEnv "QUERY_STRING")
                 (\ e -> if IOError.isDoesNotExistError e
                            then return ""
                            else ioError e)


type Param = (String, String)
type Params = [Param]

getParam :: Params -> String -> String
getParam params name = case lookup name params of
                            Just v  -> v
                            Nothing -> ""

parseParams :: String -> Params
parseParams = map parseParam . paramList
    where
        paramList :: String -> [String]
        paramList "" = []
        paramList s = case TextUtil.splitString '&' s of
                           (_, [])  -> [s]
                           (xs, ys) -> xs:(paramList ys)

parseParam :: String -> Param
parseParam = decodeValue . TextUtil.splitString '='
    where
        decodeValue :: Param -> Param
        decodeValue (name, value) = (name, HTMLUtil.decodeURL value)
