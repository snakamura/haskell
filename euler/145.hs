import Data.Char
import Data.List

main = print value

value = sum $ map count [1..9]

count n | n <= 3 || even n || ((n - 3) `mod` 4 == 0) = length $ filter reversible [10^(n - 1)..10^n - 1]
        | otherwise                                  = 0

reversible n | n `mod` 10 == 0           = False
             | odd n == oddChar (head s) = False
             | even l                    = reversibleEven
             | ((l - 3) `mod` 4) == 0    = reversibleOdd
             | otherwise                 = False
 where
     s = show n
     l = length s
     reversibleEven = let (p, n) = splitAt (l `div` 2) s
                      in and $ zipWith oddNotCarry p (reverse n)
     reversibleOdd = let (p, n) = splitAt (l `div` 2) s
                     in (digitToInt (head n) < 5) &&
                        (and $ zipWith3 apply (cycle [oddCarry, evenNotCarry]) p (reverse (tail n)))
     apply f x y = f x y

oddCarry x y = let z = add x y
               in z >= 10 && odd z
oddNotCarry x y = let z = add x y
                  in z < 10 && odd z
evenNotCarry x y = let z = add x y
                   in z < 9 && even z
add x y = digitToInt x + digitToInt y
oddChar = odd . digitToInt

{-
reversible n | n `mod` 10 == 0  = False
             | otherwise        = and $ map oddChar $ show $ n + read (reverse (show n))
-}
