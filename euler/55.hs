import Data.Char

value = length $ filter lychrel [1..10000]

lychrel n = null $ dropWhile (not . palindromic) $ take 50 $ tail $ iterate ra n

ra n = n + r
 where
     r = read $ reverse $ show n

palindromic n = show n == reverse (show n)
