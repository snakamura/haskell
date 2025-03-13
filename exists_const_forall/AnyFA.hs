module AnyFA where

import Data.Kind

type f ~> g = forall x. f x -> g x

fmap' :: Functor f => f a -> (a -> b) -> f b
fmap' = flip fmap

fmap'' :: Functor f => f a -> (a -> x) -> f x
fmap'' = fmap'

type AnyFA :: (Type -> Type) -> Type -> Type
newtype AnyFA f a = MkAnyFA (forall x. (a -> x) -> f x)

fmap''' :: Functor f => f a -> AnyFA f a
fmap''' fa = MkAnyFA (fmap'' fa)

fmap'''' :: Functor f => f ~> AnyFA f
fmap'''' fa = MkAnyFA (fmap'' fa)
