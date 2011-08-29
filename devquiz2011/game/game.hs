module Main (main) where

import Data.List (elemIndex, findIndex, sortBy, subsequences, transpose)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

main :: IO ()
main = interact $ writeOutputs . map solve . readInputs

solve :: [Int] -> Int
solve input = let m = map generate input
                  Just maxIndex = findIndex and $ transpose m
                  e = estimate input
                  candidates = filter (\c -> step c < e) $ tail $ subsequences [0..maxIndex]
              in minimum $ e:(map step $ filter (apply m) candidates)
    where
      step c = last c + length c
      apply [] _           = True
      apply _  []          = False
      apply m  (i:indices) = apply (filter (not . (!! i)) m) indices

estimate :: [Int] -> Int
estimate input = let m = sortBy (flip $ comparing firstDividable) $ map generate input
                 in solve' (firstDividable $ head m) m
    where
      solve' acc []    = acc
      solve' acc (k:m) = solve' (acc + 1) $ remove (firstDividable k) m
      remove v = filter $ not . (!! v)
      firstDividable = fromJust . elemIndex True

generate :: Int -> [Bool]
generate = map ((== 0) . flip mod 5) . iterate (flip div 2)

readInputs :: String -> [[Int]]
readInputs = map (map read . words) . pickup . tail . lines
    where
      pickup (_:d:r) = d:pickup r
      pickup _       = []
       
writeOutputs :: [Int] -> String
writeOutputs = unlines . map show
