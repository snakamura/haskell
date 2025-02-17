{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflection10 where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

type ReifiableConstraint :: (Type -> Constraint) -> Constraint
class ReifiableConstraint c where
  type Dict c a :: Type

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap {unwrap :: a}

withDict :: forall c a. Dict c a -> (forall k (s :: k). (Reifies s (Dict c a)) => Wrap s a) -> a
withDict dict value = reify dict $ \(_ :: Proxy s) -> unwrap (value :: Wrap s a)

type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance ReifiableConstraint Semigroup where
  type Dict Semigroup a = DictMonoid a

instance ReifiableConstraint Monoid where
  type Dict Monoid a = DictMonoid a

instance (Reifies s (DictMonoid a)) => Semigroup (Wrap s a) where
  (Wrap a1) <> (Wrap a2) = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))

v :: (Reifies s (DictMonoid Int)) => Wrap s Int
v = mempty <> Wrap 10 <> Wrap 20

v1 :: Int
v1 = withDict @Monoid (DictMonoid 0 (+)) v

v2 :: Int
v2 = withDict @Monoid (DictMonoid 1 (*)) v
