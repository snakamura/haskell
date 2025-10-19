module HList (HList(..)) where

import Data.Kind

type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr `HCons`
