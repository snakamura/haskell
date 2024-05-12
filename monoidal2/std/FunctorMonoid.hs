module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation

type FunctorMonoidalCategory ::
  BinaturalTransformationType ->
  Constraint
class (BinaturalTransformation t, Functor (FunctorUnit t)) => FunctorMonoidalCategory t where
  type FunctorUnit t :: FunctorType

  assoc ::
    (Functor f, Functor g, Functor h) =>
    t f (t g h) ~> t (t f g) h
  assocInv ::
    (Functor f, Functor g, Functor h) =>
    t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t (FunctorUnit t) f ~> f
  leftInv :: (Functor f) => f ~> t (FunctorUnit t) f

  right :: (Functor f) => t f (FunctorUnit t) ~> f
  rightInv :: (Functor f) => f ~> t f (FunctorUnit t)

type FunctorMonoidObject :: BinaturalTransformationType -> FunctorType -> Constraint
class (FunctorMonoidalCategory t, Functor f) => FunctorMonoidObject t f where
  mu :: t f f ~> f
  eta :: (FunctorUnit t) ~> f
