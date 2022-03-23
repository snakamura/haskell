{-# LANGUAGE FlexibleInstances,
             RankNTypes,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
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

instance MonoidF'' ((->) r) where
    type Product'' ((->) r) = Compose ((->) r) ((->) r)
    type Unit'' ((->) r) = Identity
    muF'' (Compose f) e = f e e
    etaF'' (Identity x) = const x

newtype R r a = R (r -> a)

instance MonoidF'' (R r) where
    type Product'' (R r) = Compose (R r) (R r)
    type Unit'' (R r) = Identity
    muF'' (Compose (R f)) = R $ \e -> let R g = f e in g e
    etaF'' (Identity x) = R $ const x

instance Monoid w => MonoidF'' ((,) w) where
    type Product'' ((,) w) = Compose ((,) w) ((,) w)
    type Unit'' ((,) w) = Identity
    muF'' (Compose (w2, (w1, x))) = (w1 <> w2, x)
    etaF'' (Identity x) = (mempty, x)

newtype W w a = W (a, w)

instance Monoid w => MonoidF'' (W w) where
    type Product'' (W w) = Compose (W w) (W w)
    type Unit'' (W w) = Identity
    muF'' (Compose (W (W (x, w1), w2))) = W (x, w1 <> w2)
    etaF'' (Identity x) = W (x, mempty)


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
