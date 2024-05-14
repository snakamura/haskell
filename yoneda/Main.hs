module Main where

import Data.Functor.Yoneda
import SList qualified

list :: [Int]
list = [1 .. 1000000]

slist :: SList.SList Int
slist = SList.from list

test, test' :: (Functor f, Show a) => f a -> f Int
test = fmap length . fmap show . fmap length . fmap show
test' = fmap (length . show . length . show)

testYoneda :: IO ()
testYoneda = do
  print $ length $ test list
  print $ SList.length $ test slist
  print $ SList.length $ lowerYoneda $ test $ liftYoneda slist
  print $ SList.length $ test' slist

main :: IO ()
main = do
  testYoneda
