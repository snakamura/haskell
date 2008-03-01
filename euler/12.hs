import Data.List

value = last $ head $ dropWhile ((< 500) . length) $ map divisors $ map f [1..]
 where
     f n = sum [1..n]

divisors n = let d = [ x | x <- [2..n `div` 2], n `mod` x == 0 ]
             in case d of
                 []  -> [1, n]
                 d:_ -> let r = divisors (n `div` d)
                        in nub $ sort $ 1:d:r ++ map (*d) (tail r)
