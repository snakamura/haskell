module HTMLUtil (escapeHtml,
                 encodeURLComponent,
                 decodeURL)
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
            | isAlphaNum c = [c]
            | c == '_'     = [c]
            | otherwise    = '%':(intToDigit $ n `div` 16):(intToDigit $ n `mod` 16):[]
            where
                n = ord c

decodeURL :: String -> String
decodeURL "" = ""
decodeURL ('%':r@(c1:c2:s)) | isHexDigit c1 && isHexDigit c2 = (decodeChar c1 c2):(decodeURL s)
                            | otherwise                      = '%':(decodeURL r)
    where
        decodeChar :: Char -> Char -> Char
        decodeChar c1 c2 = chr ((digitToInt c1)*16 + digitToInt c2)
decodeURL (c:s) | c == '+'  = ' ':(decodeURL s)
                | otherwise = c:(decodeURL s)
