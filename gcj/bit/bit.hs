module Main where

import Data.Char (chr, ord)
import Data.List (mapAccumR)
import Numeric (readInt, showIntAtBase)
import Text.Printf (printf)


main :: IO ()
main = interact (writeOutput . map solve . readInput)


readInput :: (Integral a, Read a) => String -> [a]
readInput = map read . tail . lines


writeOutput :: [Int] -> String
writeOutput = unlines . zipWith (printf "Case #%d: %d") [1 :: Int ..]


solve :: Integral a => a -> Int
solve = _maxBit


maxBit :: Integral a => a -> Int
maxBit n = let a = gen $ showBin n
           in bit a + bit (n - a)


gen :: Integral a => String -> a
gen s | elem '0' s = case dropWhile (== '0') $ tail s of
                       "" -> 1
                       s2 -> readBin $ '0':snd (mapAccumR f 0 s2)
      | otherwise  = 0
    where
      f 0 '0' = (2, '1')
      f 0 '1' = (1, '0')
      f 1 '0' = (2, '1')
      f 1 '1' = (1, '0')
      f 2 c   = (2, c  )


_maxBit :: Integral a => a -> Int
_maxBit n = maximum $ candidates n
    where
      candidates n = [ bit a + bit (n - a) | a <- [0 .. n `div` 2] ]


bit :: Integral a => a -> Int
bit 0 = 0
bit n | even n = bit (n `div` 2)
      | otherwise = 1 + bit (n `div` 2)


showBin :: Integral a => a -> String
showBin n = showIntAtBase 2 (chr . (ord '0' +)) n ""


readBin :: Integral a => String -> a
readBin = fst . head . readInt 2 (flip elem ['0','1']) ((subtract $ ord '0') . ord)
