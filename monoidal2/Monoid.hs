module Monoid where

import Data.Kind
import Functor
import Prelude ()

type Monoidal :: BifunctorType -> Constraint
class (Bifunctor t) => Monoidal t where
  type Unit t :: Type

  assoc :: t a (t a a) -> t (t a a) a
  assocInv :: t (t a a) a -> t a (t a a)

  left :: t (Unit t) a -> a
  leftInv :: a -> t (Unit t) a

  right :: t a (Unit t) -> a
  rightInv :: a -> t a (Unit t)

type Monoid :: Type -> Constraint
class (Monoidal (Tensor a)) => Monoid a where
  type Tensor a :: BifunctorType
  mu :: Tensor a a a -> a
  eta :: Unit (Tensor a) -> a
