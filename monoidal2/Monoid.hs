module Monoid where

import Data.Kind
import Functor
import Prelude ()

type Monoid :: Type -> Constraint
class (Bifunctor (Tensor a)) => Monoid a where
  type Tensor a :: Type -> Type -> Type
  type Unit a :: Type
  mu :: Tensor a a a -> a
  eta :: Unit a -> a
