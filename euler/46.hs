import Control.Monad

import Prime

value = head values

values = do n <- [9,11..]
            guard $ not $ isPrime primes n
            guard $ not $ test n
            return n

test n = let p = takeWhile (< n) primes
         in or $ map (\p -> isSquare $ (n - p) `div` 2) p

isSquare n = let m = floor $ sqrt $ fromIntegral n
             in m*m == n
