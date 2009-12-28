import Data.List
import Data.Maybe

main = print value

value = fromJust (findIndex f fibs) + 1

fibs = 1:1:zipWith (+) fibs (tail fibs)

f n = pandigit (read $ take 9 $ show n) && pandigit (n `mod` 10^9)

pandigit n = f n == 0x3FE
  where
    f 0 = 0
    f n = let (q, r) = quotRem n 10
          in 2^r + f q
