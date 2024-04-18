module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation
import Prelude ()

type FunctorMonoidal ::
  BinaturalTransformationType ->
  FunctorType ->
  Constraint
class
  (BinaturalTransformation t) =>
  FunctorMonoidal t u
    | t -> u
  where
  assoc ::
    (Functor f, Functor g, Functor h) =>
    t f (t g h) ~> t (t f g) h
  assocInv ::
    (Functor f, Functor g, Functor h) =>
    t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t u f ~> f
  leftInv :: (Functor f) => f ~> t u f

  right :: (Functor f) => t f u ~> f
  rightInv :: (Functor f) => f ~> t f u

type FunctorMonoid ::
  BinaturalTransformationType ->
  FunctorType ->
  FunctorType ->
  Constraint
class
  ( FunctorMonoidal t u,
    Functor u,
    Functor f
  ) =>
  FunctorMonoid t u f
  where
  mu :: t f f ~> f
  eta :: u ~> f
