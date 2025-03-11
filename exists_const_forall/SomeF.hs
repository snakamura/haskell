module SomeF where

import Data.Kind

type SomeF :: (k -> Type) -> Type
data SomeF f where
  MkSomeF :: f a -> SomeF f

lengthSomeF :: SomeF [] -> Int
lengthSomeF (MkSomeF l) = length l

someFValue :: SomeF []
someFValue = MkSomeF [1 :: Int]

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

fmap' :: Functor f => f a -> (a -> b) -> f b
fmap' = flip fmap

fmap'' :: Functor f => f x -> (x -> a) -> f a
fmap'' = fmap'

type SomeFA :: (Type -> Type) -> Type -> Type
data SomeFA f a where
  MkSomeFA :: f x -> (x -> a) -> SomeFA f a

fmap''' :: Functor f => SomeFA f a -> f a
fmap''' = \(MkSomeFA fa g) -> fmap'' fa g

fmap'''' :: Functor f => SomeFA f ~> f
fmap'''' = \(MkSomeFA fa g) -> fmap'' fa g

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' = liftA2

liftA2'' :: Applicative f => (x -> y -> a) -> f x -> f y -> f a
liftA2'' = liftA2'

data SomeFA2 f a where
  MkSomeFA2 :: (x -> y -> a) -> f x -> f y -> SomeFA2 f a

liftA2''' :: Applicative f => SomeFA2 f a -> f a
liftA2''' (MkSomeFA2 g fx fy) = liftA2'' g fx fy
