module SomeFA2 where

import Data.Kind

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' = liftA2

liftA2'' :: Applicative f => (x -> y -> a) -> f x -> f y -> f a
liftA2'' = liftA2'

liftA2''' :: Applicative f => (x -> y -> a, f x, f y) -> f a
liftA2''' (g, fx, fy) = liftA2' g fx fy

type SomeFA2 :: (Type -> Type) -> Type -> Type
data SomeFA2 f a = forall x y. MkSomeFA2 (x -> y -> a) (f x) (f y)

liftA2'''' :: Applicative f => SomeFA2 f a -> f a
liftA2'''' (MkSomeFA2 g fx fy) = liftA2''' (g, fx, fy)

type Day :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data Day f g a = forall x y. Day (x -> y -> a) (f x) (g y)

dap :: Applicative f => Day f f a -> f a
dap (Day h fx fy) = liftA2 h fx fy
