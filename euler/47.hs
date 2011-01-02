import Data.List

import Prime

value = fst $ head $ head $ filter (test . map snd) (tails $ zip [2..] factors)

test l = let v = take 4 l
         in map (length . nub) v == [4, 4, 4, 4]

factors = map factorize [2..]

factorize n = let m = head $ filter (\p -> n `mod` p == 0) primes
              in if n == m then
                   [m]
                 else
                   m:(factors !! fromInteger ((n `div` m) - 2))
