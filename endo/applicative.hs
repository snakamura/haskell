{-# LANGUAGE TypeFamilies,
             UndecidableInstances
#-}

import Control.Applicative (Applicative(liftA2))
import Data.Functor.Day (Day(Day))
import Data.Functor.Identity (Identity(Identity))
import Data.Kind (Constraint, Type)

type f ~> g = forall a. f a -> g a

type MonoidF :: (Type -> Type) -> Constraint
class MonoidF f where
    type ProductF f :: Type -> Type
    type UnitF f :: Type -> Type
    muF :: ProductF f ~> f
    etaF :: UnitF f ~> f

instance MonoidF Maybe where
    type ProductF Maybe = Day Maybe Maybe
    type UnitF Maybe = Identity
    muF :: Day Maybe Maybe a -> Maybe a
    muF (Day (Just x) (Just y) f) = Just $ f x y
    muF _ = Nothing
    etaF :: Identity a -> Maybe a
    etaF (Identity x) = Just x

instance MonoidF [] where
    type ProductF [] = Day [] []
    type UnitF [] = Identity
    muF :: Day [] [] a -> [a]
    muF (Day xs ys f) = [f x y | x <- xs, y <- ys]
    etaF :: Identity a -> [a]
    etaF (Identity x) = [x]

instance MonoidF ((->) r) where
    type ProductF ((->) r) = Day ((->) r) ((->) r)
    type UnitF ((->) r) = Identity
    muF :: Day ((->) r) ((->) r) a -> r -> a
    muF (Day f g h) r = h (f r) (g r)
    etaF :: Identity a -> r -> a
    etaF (Identity x) = const x

instance Monoid w => MonoidF ((,) w) where
    type ProductF ((,) w) = Day ((,) w) ((,) w)
    type UnitF ((,) w) = Identity
    muF :: Day ((,) w) ((,) w) a -> (w, a)
    muF (Day (w1, x) (w2, y) f) = (w1 <> w2, f x y)
    etaF :: Identity a -> (w, a)
    etaF (Identity x) = (mempty, x)

type Applicative' :: (Type -> Type) -> Constraint
class Functor f => Applicative' f where
    pair' :: f a -> f b -> f (a, b)
    pure' :: a -> f a

instance (Functor f, MonoidF f, ProductF f ~ Day f f, UnitF f ~ Identity) => Applicative' f where
    pair' fa fb = muF $ Day fa fb (,)
    pure' = etaF . Identity

instance (Functor f, MonoidF f, ProductF f ~ Day f f, UnitF f ~ Identity) => Applicative f where
    fa <*> fb = muF $ Day fa fb id
    pure = etaF . Identity
    liftA2 f fa fb = muF $ Day fa fb f
