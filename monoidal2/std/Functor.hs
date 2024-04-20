module Functor where

import Data.Kind

type FunctorType :: Type
type FunctorType = Type -> Type

type BifunctorType :: Type
type BifunctorType = Type -> Type -> Type
