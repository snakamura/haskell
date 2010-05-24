import Control.Monad
import Data.List

import Prime

value = sum $ map listToInt values

values = filter f $ pandigitals 9
  where
    checks = take 7 $ zip [2..] primes
    f p = and $ map (uncurry $ check p) checks

check v digit divisor = let m = take 3 $ drop (digit - 1) v
                        in listToInt m `mod` divisor == 0

pandigitals 0 = [[0]]
pandigitals n = do p <- pandigitals (n - 1)
                   zipWith (\x y -> x ++ [n] ++ y) (inits p) (tails p)

listToInt x = listToInt' $ reverse x
  where
    listToInt' [] = 0
    listToInt' (x:xs) = x + 10*listToInt' xs
