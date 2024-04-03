module Monoid where

import Data.Kind

type Monoid :: Type -> Constraint
class Monoid a where
  type Tensor a :: Type
  type Unit a :: Type
  mu :: Tensor a -> a
  eta :: Unit a -> a
