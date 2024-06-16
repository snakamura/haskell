module Monoid where

import Data.Bifunctor
import Data.Kind
import Functor

type MonoidalCategory :: BifunctorType -> Constraint
class (Bifunctor t) => MonoidalCategory t where
  type Unit t :: Type

  assoc :: t a (t a a) -> t (t a a) a
  assocInv :: t (t a a) a -> t a (t a a)

  left :: t (Unit t) a -> a
  leftInv :: a -> t (Unit t) a

  right :: t a (Unit t) -> a
  rightInv :: a -> t a (Unit t)

type MonoidObject :: BifunctorType -> Type -> Constraint
class (MonoidalCategory t) => MonoidObject t a where
  mu :: t a a -> a
  eta :: Unit t -> a

type MonoidHomomorphism :: Type -> Type -> Type
newtype MonoidHomomorphism m1 m2 = Hom (m1 -> m2)

class
  ( MonoidalCategory t,
    MonoidObject t m1,
    MonoidObject t m2
  ) =>
  MonoidHomomorphismLaws t m1 m2
  where
  preserveIdentity :: MonoidHomomorphism m1 m2 -> Bool
  preserveAppend :: MonoidHomomorphism m1 m2 -> t m1 m1 -> Bool
