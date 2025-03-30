module SomeF where

import Data.Kind

type SomeF :: (k -> Type) -> Type
data SomeF f = forall a. MkSomeF (f a)

someFValue :: SomeF []
someFValue = MkSomeF [1 :: Int]

fromSomeF :: SomeF [] -> Int
fromSomeF (MkSomeF l) = length l

toSomeF :: Int -> SomeF []
toSomeF n = MkSomeF [n]

forward :: (SomeF f -> a) -> (forall x. f x -> a)
forward g = \fx -> g (MkSomeF fx)

backward :: (forall x. f x -> a) -> (SomeF f -> a)
backward h = \(MkSomeF fx) -> h fx

newtype Const a x = MkConst a

forward' :: (SomeF f -> a) -> (forall x. f x -> Const a x)
forward' g = \fx -> MkConst (g (MkSomeF fx))

backward' :: (forall x. f x -> Const a x) -> (SomeF f -> a)
backward' h = \(MkSomeF fx) -> let MkConst a = h fx in a

type f ~> g = forall x. f x -> g x

leftAdjunct :: (SomeF f -> a) -> (f ~> Const a)
leftAdjunct g = \fx -> MkConst (g (MkSomeF fx))

rightAdjunct :: (f ~> Const a) -> (SomeF f -> a)
rightAdjunct h = \(MkSomeF fx) -> let MkConst a = h fx in a
