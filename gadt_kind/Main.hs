module Main where

import Data.Kind

main :: IO ()
main = pure ()

data X a where
  XInt :: X Int

data Y1 f where
  Y1Maybe :: (a -> Maybe a) -> Y1 Maybe

y1 :: Y1 Maybe
y1 = Y1Maybe Just

type Y2 :: (k -> Type) -> Type
data Y2 f where
  Y2Maybe :: (a -> Maybe a) -> Y2 Maybe

y2 :: Y2 Maybe
y2 = Y2Maybe Just

type data S = S1 | S2

type T :: S -> Type
data T s = T

type Y3 :: (k -> Type) -> Type
data Y3 f where
  Y3Maybe :: (a -> Maybe a) -> Y3 Maybe
  Y3T :: T s -> Y3 T

y3_1 :: Y3 Maybe
y3_1 = Y3Maybe Just

y3_2 :: Y3 T
y3_2 = Y3T T
