module Some where

import Data.Kind

type Some :: Type
data Some where
  MkSome :: a -> Some

someValue :: Some
someValue = MkSome (1 :: Int)

fromSome :: Some -> Some
fromSome (MkSome a) = MkSome (id a)

toSome :: Int -> Some
toSome n = MkSome n
