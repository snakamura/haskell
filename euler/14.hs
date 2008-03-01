import Control.Arrow
import Data.List
import Data.Ord

value = maximumBy' (comparing snd) $ map (id &&& cl) [1..1000000]

maximumBy' cmp = foldl1' max
 where
     max x y = case cmp x y of
                   GT -> x
                   _  -> y

c n | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3*n + 1

cl = (+1) . length . takeWhile (/= 1) . iterate c
