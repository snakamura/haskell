module Monoid where

import Data.Bifunctor
import Data.Kind
import Functor

type MonoidalCategory :: BifunctorType -> Type -> Constraint
class (Bifunctor t) => MonoidalCategory t u | t -> u where
  assoc :: t a (t a a) -> t (t a a) a
  assocInv :: t (t a a) a -> t a (t a a)

  left :: t u a -> a
  leftInv :: a -> t u a

  right :: t a u -> a
  rightInv :: a -> t a u

type MonoidObject :: BifunctorType -> Type -> Type -> Constraint
class (MonoidalCategory t u) => MonoidObject t u a where
  mu :: t a a -> a
  eta :: u -> a
