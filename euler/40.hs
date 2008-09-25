import Data.Char

value = product $ map (digitToInt . (l !!) . (flip (-) 1)) [1, 10, 100, 1000, 10000, 100000, 1000000]

l = concat $ map show [1..]
