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
solve (m, w, cuts) = let c = foldl' f [1..m] cuts
                     in c !! (w - 1)
    where
      f cards (a, b) = shuffle a b cards


shuffle :: Int -> Int -> [Int] -> [Int]
shuffle a b cards = let (c1, rest) = splitAt (a - 1) cards
                        (c2, c3) = splitAt b rest
                    in c2 ++ c1 ++ c3
