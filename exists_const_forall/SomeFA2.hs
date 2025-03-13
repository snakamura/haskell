module SomeFA2 where

import Data.Kind

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' = liftA2

liftA2'' :: Applicative f => (x -> y -> a) -> f x -> f y -> f a
liftA2'' = liftA2'

liftA2''' :: Applicative f => (x -> y -> a, f x, f y) -> f a
liftA2''' (g, fx, fy) = liftA2' g fx fy

type SomeFA2 :: (Type -> Type) -> Type -> Type
data SomeFA2 f a where
  MkSomeFA2 :: (x -> y -> a) -> f x -> f y -> SomeFA2 f a

liftA2'''' :: Applicative f => SomeFA2 f a -> f a
liftA2'''' (MkSomeFA2 g fx fy) = liftA2''' (g, fx, fy)
