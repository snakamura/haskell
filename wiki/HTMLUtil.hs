module HTMLUtil (escapeHtml,
                 encodeURLComponent)
    where

import Data.Char

escapeHtml :: String -> String
--escapeHtml = foldr ((++) . escapeHtmlChar) []
escapeHtml = concatMap escapeHtmlChar
    where
        escapeHtmlChar :: Char -> String
        escapeHtmlChar c | c == '<'  = "&lt;"
                         | c == '>'  = "&gt;"
                         | c == '&'  = "&amp;"
                         | c == '\"' = "&quot;"
                         | otherwise = [c]

encodeURLComponent :: String -> String
encodeURLComponent = concatMap encodeURLComponentChar
    where
        encodeURLComponentChar :: Char -> String
        encodeURLComponentChar c
            | 'A' <= c && c <= 'Z' = [c]
            | 'a' <= c && c <= 'z' = [c]
            | '0' <= c && c <= '9' = [c]
            | c == '_'             = [c]
            | otherwise            = '%':(intToDigit $ n `div` 16):(intToDigit $ n `mod` 16):[]
            where
                n = ord c
