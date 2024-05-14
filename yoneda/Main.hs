module Main where

import Data.Functor.Coyoneda
import Data.Functor.Yoneda
import NList qualified
import SList qualified

list :: [Int]
list = [1 .. 1000000]

slist :: SList.SList Int
slist = SList.from list

nlist :: NList.NList Int
nlist = NList.from list

test, test' :: (Functor f, Show a) => f a -> f Int
test = fmap length . fmap show . fmap length . fmap show
test' = fmap (length . show . length . show)

testYoneda :: IO ()
testYoneda = do
  print $ length $ test list
  print $ SList.length $ test slist
  print $ SList.length $ lowerYoneda $ test $ liftYoneda slist
  print $ SList.length $ test' slist

applyTest :: Show a => f a -> Coyoneda f Int
applyTest c = test $ liftCoyoneda c

testCoyoneda :: IO ()
testCoyoneda = do
  print $ length $ lowerCoyoneda $ applyTest list
  print $ SList.length $ lowerCoyoneda $ applyTest slist
  -- print $ NList.length $ lowerCoyoneda $ applyTest nlist
  print $ length $ lowerCoyoneda $ hoistCoyoneda NList.to $ applyTest nlist


main :: IO ()
main = do
  testYoneda
  testCoyoneda

