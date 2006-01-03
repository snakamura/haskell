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
        decodeValue (name, value) = (name, decode value)
        decode :: String -> String
        decode "" = ""
        decode ('%':r@(c1:c2:s))
            | isHexDigit c1 && isHexDigit c2 = (decodeChar c1 c2):(decode s)
            | otherwise                      = '%':(decode r)
        decode (c:s) | c == '+'  = ' ':(decode s)
                     | otherwise = c:(decode s)
        decodeChar :: Char -> Char -> Char
        decodeChar c1 c2 = chr ((digitToInt c1)*16 + digitToInt c2)


splitString :: Char -> String -> (String, String)
splitString c s = case break (c ==) s of
                       (_, [])    -> (s, [])
                       (xs, _:ys) -> (xs, ys)
