module Some where

import Data.Kind

type Some :: Type
data Some where
  MkSome :: a -> Some

someId :: Some -> Some
someId (MkSome a) = MkSome (id a)

someValue :: Some
someValue = MkSome (1 :: Int)
