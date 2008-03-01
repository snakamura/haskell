import Data.List

value = sum $ filter amicable [1..10000]

amicable n = let d = sum $ properDivisors n
             in d /= n && sum (properDivisors d) == n

--properDivisors n = [ x | x <- [1..n `div` 2], n `mod` x == 0 ]
properDivisors n = init $ divisors n

divisors n = let d = [ x | x <- [2..n `div` 2], n `mod` x == 0 ]
             in case d of
                 []  -> [1, n]
                 d:_ -> let r = divisors (n `div` d)
                        in nub $ sort $ 1:d:r ++ map (*d) (tail r)
