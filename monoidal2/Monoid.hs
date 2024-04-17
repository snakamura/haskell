module Monoid where

import Data.Kind
import Functor
import Prelude ()

type Monoidal :: BifunctorType -> Type -> Constraint
class (Bifunctor t) => Monoidal t u | t -> u where
  assoc :: t a (t a a) -> t (t a a) a
  assocInv :: t (t a a) a -> t a (t a a)

  left :: t u a -> a
  leftInv :: a -> t u a

  right :: t a u -> a
  rightInv :: a -> t a u

type Monoid :: BifunctorType -> Type -> Type -> Constraint
class (Monoidal t u) => Monoid t u a where
  mu :: t a a -> a
  eta :: u -> a
