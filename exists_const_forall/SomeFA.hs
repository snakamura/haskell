module SomeFA where

import Data.Kind

type f ~> g = forall x. f x -> g x

fmap' :: Functor f => f a -> (a -> b) -> f b
fmap' = flip fmap

fmap'' :: Functor f => f x -> (x -> a) -> f a
fmap'' = fmap'

fmap''' :: Functor f => (f x, x -> a) -> f a
fmap''' = uncurry fmap''

type SomeFA :: (Type -> Type) -> Type -> Type
data SomeFA f a where
  MkSomeFA :: f x -> (x -> a) -> SomeFA f a

fmap'''' :: Functor f => SomeFA f a -> f a
fmap'''' = \(MkSomeFA fa g) -> fmap''' (fa,  g)

fmap''''' :: Functor f => SomeFA f ~> f
fmap''''' = \(MkSomeFA fa g) -> fmap''' (fa, g)

type Coyoneda :: (Type -> Type) -> Type -> Type
data Coyoneda f a where
  Coyoneda :: (x -> a) -> f x -> Coyoneda f a

lowerCoyoneda :: Functor f => Coyoneda f ~> f
lowerCoyoneda (Coyoneda g fa) = g <$> fa
