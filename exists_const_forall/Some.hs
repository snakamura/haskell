module Some where

import Data.Kind

type Some :: Type
data Some = forall a. MkSome a

someValue :: Some
someValue = MkSome (1 :: Int)

fromSome :: Some -> Some
fromSome (MkSome a) = MkSome (id a)

toSome :: Int -> Some
toSome n = MkSome n

forward :: (Some -> a) -> (forall x. x -> a)
forward g = \x -> g (MkSome x)

backward :: (forall x. x -> a) -> (Some -> a)
backward h = \(MkSome x) -> h x
