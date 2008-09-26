import Data.List
import Data.Ord

value = let r = [1..999]
        in fst $ maximumBy (comparing snd) $ zip r $ map values r

values n = let t = n `div` 3
               h = n `div` 2
           in length $ [ (a, b, c) | a <- [1..h],
                                     b <- [a..h],
                                     let c = n - a - b,
                                     c >= b,
                                     c >= t,
                                     c <= h,
                                     isRightAngle a b c ]

isRightAngle a b c = a^2 + b^2 == c^2
