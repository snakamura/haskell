import Control.Monad
import System.IO

value = do dat <- liftM parse $ readFile "67.txt"
           return $ maximum $ last $ iterate (next dat) [[dat !! 0 !! 0]] !! (length dat - 1)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

next :: (Num a, Ord a) => [[a]] -> [[a]] -> [[a]]
next dat sum = let n = length sum
                   l = map (sumAt dat sum n) [0..n]
               in sum ++ [l]

sumAt :: (Num a, Ord a) => [[a]] -> [[a]] -> Int -> Int -> a
sumAt dat sum n m = let p = if m == 0 then
                                sum !! (n - 1) !! m
                            else if m == n then
                                sum !! (n - 1) !! (m - 1)
                            else
                                max (sum !! (n - 1) !! (m - 1)) (sum !! (n - 1) !! m)
                    in p + dat !! n !! m
