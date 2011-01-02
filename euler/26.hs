import Data.List
import Data.Ord

value = snd $ maximumBy (comparing fst) $ zip (map f [1..999]) [1..999]
    where
      f n = shortestCycle $ show $ 10^10000 `div` n

shortestCycle s = let l = map (isCycle s) [1..length s `div` 2]
                  in fmap snd $ find fst $ zip l [1..]

isCycle s n = and $ zipWith (==) (take n s) (drop n s)
