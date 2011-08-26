module Main (main) where

import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

main :: IO ()
main = interact $ writeOutputs . map solve . readInputs

solve :: [Int] -> Int
solve input = let m = sortBy (flip $ comparing firstDividable) $ map generate input
              in solve' (firstDividable $ head m) m
    where
      generate = map ((== 0) . flip mod 5) . iterate (flip div 2)
      solve' acc []    = acc
      solve' acc (k:m) = solve' (acc + 1) $ remove (firstDividable k) m
      remove v = filter $ not . (!! v)
      firstDividable = fromJust . elemIndex True

readInputs :: String -> [[Int]]
readInputs = map (map read . words) . pickup . tail . lines
    where
      pickup (_:d:r) = d:pickup r
      pickup _       = []
       
writeOutputs :: [Int] -> String
writeOutputs = unlines . map show
