import Data.List

value = length $ filter f calendar
    where
      f (c:_) = c == 1

calendar = drop (365 `div` 7) $ split 7 $ 0:concatMap (uncurry days) [ (y, m) | y <- [1900..2000], m <- [1..12] ]

days year month = [1..lastDay]
    where
      lastDay = if month == 2 then
                    if leap year then
                        29
                    else
                        28
                else if month `elem` [4, 6, 9, 11] then
                    30
                else
                    31

leap year = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year ` mod` 400 == 0)

split _ [] = []
split n l  = let (a, d) = splitAt n l
             in a:split n d
