module HTMLUtil where

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

