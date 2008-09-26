import Data.List

main = print value

value = sum $ diff [1..28123] $ nub $ sort values

diff []      _                   = []
diff s       []                  = s
diff (c1:s1) (c2:s2) | c1 == c2  = diff s1 s2
                     | otherwise = c1:diff s1 (c2:s2)

values = [ s | x <- l, y <- l, x <= y, let s = x + y, s <= 28123 ]
    where
      l = takeWhile (<= 28123) abandantNumbers

abandantNumbers = filter isAbandant [1..]

isAbandant n = sumOfProperDividers n > n

sumOfProperDividers = sum . properDividers

properDividers n = 1:filter (\m -> n `mod` m == 0) [2..n `div` 2]
