import Data.Char

value = sum $ filter f [2..999999]
    where
      f n = sfp5 n == n

sfp5 n = sum $ map (^5) $ map digitToInt $ show n
