module Monoid where

import Data.Kind

type Monoid :: Type -> Constraint
class Monoid a where
  type Tensor a :: Type
  type Id a :: Type
  mu :: Tensor a -> a
  eta :: Id a -> a
