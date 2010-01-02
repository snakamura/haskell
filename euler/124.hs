import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Prime

main = print value

value = fst $ sortBy (comparing snd) (take 100000 $ zip [1..] rads) !! (10000 - 1)

rads :: [Int]
rads = rads' 1 Map.empty

rads' :: Int -> Map Int Int -> [Int]
rads' n map = let m = fromMaybe (rad n map) $ Map.lookup n map
              in m:rads' (n + 1) (Map.insert n m map)

rad :: Int -> Map Int Int -> Int
rad 1 _   = 1
rad n map = let x = divider n
                r = fromJust $ Map.lookup (n `div` x) map
            in case (n `div` x) `mod` x of
                 0 -> r
                 _ -> r * x

divider :: Int -> Int
divider n = fromInteger $ head $ filter (\x -> (fromIntegral n `mod` x) == 0) primes
