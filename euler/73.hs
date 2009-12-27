import Data.List
import Data.Ratio
import Prelude hiding (gcd)

main = print value

value = sum $ map (length . v) [4..12000]

v n = [ x | x <- [n `div` 3 + 1..n `div` 2], gcd x n == 1 ]

gcd a b | a > b     = case a `mod` b of
                        0 -> b
                        r -> gcd r b
        | otherwise = gcd b a
