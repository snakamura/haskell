module Main where

import Data.List (foldl')
import Text.Printf (printf)


main :: IO ()
main = interact (writeOutput . map solve . readInput)


readInput :: String -> [(Int, Int, [(Int, Int)])]
readInput = readItem . tail . lines
    where
      readItem [] = []
      readItem (h:r) = let [m, c, w] = map read $ words h
                           cuts = map readCut $ take c r
                       in (m, w, cuts):readItem (drop c r)
      readCut s = let [a, b] = map read $ words s
                  in (a, b)


writeOutput :: [Int] -> String
writeOutput = unlines . zipWith (printf "Case #%d: %d") [1 :: Int ..]


solve :: (Int, Int, [(Int, Int)]) -> Int
solve (m, w, cuts) = foldl' (uncurry . moveBack) w $ reverse cuts


moveBack :: Int -> Int -> Int -> Int
moveBack w a b | w - 1 < b         = (a - 1) + (w - 1) + 1
               | w - 1 < a - 1 + b = w - b
               | otherwise         = w
