{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Array
import Data.Char
import Data.List

main = print value

upper = 10000000

value = count (== 89) $ map v [1..upper]

v n = values ! next n

values = listArray (1, u) $ map v [1..u]
    where
      v = head . dropWhile (flip notElem [1, 89]) . iterate next
      u = 9^2*(length (show upper) - 1)

next = sum . map (^2) . map f . show
    where
      f c = ord c - ord '0'

count ps = foldl' f 0
    where
      f a n | ps n      = a + 1
            | otherwise = a
