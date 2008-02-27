import Data.Char

value = sum $ map digitToInt $ show $ foldl (*) 1 [1..100]
