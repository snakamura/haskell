{-# LANGUAGE FlexibleInstances, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Monoid (Endo(Endo))

class Monoid' a where
    mu :: (a, a) -> a
    eta :: () -> a

instance Monoid' (Endo a) where
    mu (Endo f, Endo g) = Endo $ f . g
    eta () = Endo id


class MonoidF f where
    muF :: Compose f f a -> f a
    etaF :: Identity a -> f a

instance MonoidF Maybe where
    muF (Compose (Just (Just x))) = Just x
    muF _ = Nothing
    etaF (Identity x) = Just x


class MonoidF' f where
    type Product' (f :: Type -> Type) :: Type -> Type
    type Unit' f :: Type -> Type
    muF' :: Product' f a -> f a
    etaF' :: Unit' f a -> f a

instance MonoidF' Maybe where
    type Product' Maybe = Compose Maybe Maybe
    type Unit' Maybe = Identity
    muF' (Compose (Just (Just x))) = Just x
    muF' _ = Nothing
    etaF' (Identity x) = Just x


type f ~> g = forall a. f a -> g a

class MonoidF'' f where
    type Product'' (f :: Type -> Type) :: Type -> Type
    type Unit'' f :: Type -> Type
    muF'' :: Product'' f ~> f
    etaF'' :: Unit'' f ~> f

instance MonoidF'' Maybe where
    type Product'' Maybe = Compose Maybe Maybe
    type Unit'' Maybe = Identity
    muF'' (Compose (Just (Just x))) = Just x
    muF'' _ = Nothing
    etaF'' (Identity x) = Just x


class Monad' f where
    join' :: f (f a) -> f a
    pure' :: a -> f a

{-
instance MonoidF f => Monad' f where
    join' = muF . Compose
    pure' = etaF . Identity
-}

instance (MonoidF'' f, Product'' f ~ Compose f f, Unit'' f ~ Identity) => Monad' f where
    join' = muF'' . Compose
    pure' = etaF'' . Identity
