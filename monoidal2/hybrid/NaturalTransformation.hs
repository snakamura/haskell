module NaturalTransformation where

import Data.Kind
import Functor
import Prelude ()

type NaturalTransformationType :: Type
type NaturalTransformationType = FunctorType -> FunctorType

type (~>) :: FunctorType -> FunctorType -> Type
type f ~> g = forall a. f a -> g a

type NaturalTransformation :: NaturalTransformationType -> Constraint
class NaturalTransformation t where
  ntmap ::
    (Functor f, Functor g) =>
    (f ~> g) ->
    (t f ~> t g)

type BinaturalTransformationType :: Type
type BinaturalTransformationType = FunctorType -> FunctorType -> FunctorType

type BinaturalTransformation :: BinaturalTransformationType -> Constraint
class
  (forall f. (Functor f) => NaturalTransformation (t f)) =>
  BinaturalTransformation t
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (t f g ~> t h i)
