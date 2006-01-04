module TextUtil (splitString,
                 trimString)
    where

import Data.Char


splitString :: Char -> String -> (String, String)
splitString c s = case break (c ==) s of
                       (_, [])    -> (s, [])
                       (xs, _:ys) -> (xs, ys)

trimString :: String -> String
trimString = trimRight . trimLeft
   where
       trimLeft = dropWhile isSpace
       trimRight = reverse . trimLeft . reverse
