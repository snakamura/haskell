module CGI (Method(..),
            getRequestMethod,
            getQuery,
            Param,
            Params,
            getParam,
            parseParams)
    where

import Data.Char
import System.Environment
import System.IO.Error

import HTMLUtil
import TextUtil

data Method = GET
            | POST

getRequestMethod :: IO Method
getRequestMethod = getEnv "REQUEST_METHOD" >>= return . getMethod
    where
        getMethod :: String -> Method
        getMethod method | method == "POST" = POST
                         | otherwise        = GET

getQuery :: IO String
getQuery = catch (getEnv "QUERY_STRING")
                 (\ e -> if isDoesNotExistError e
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
        paramList s = case splitString '&' s of
                           (_, [])  -> [s]
                           (xs, ys) -> xs:(paramList ys)

parseParam :: String -> Param
parseParam = decodeValue . splitString '='
    where
        decodeValue :: Param -> Param
        decodeValue (name, value) = (name, decodeURL value)
