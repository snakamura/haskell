module Functor2 where

import Data.Kind
import Functor

type Functor2Type :: Type
type Functor2Type = FunctorType -> FunctorType

type (~>) :: FunctorType -> FunctorType -> Type
type f ~> g = forall a. f a -> g a

type Functor2 :: Functor2Type -> Constraint
class Functor2 t where
  ntmap ::
    (Functor f, Functor g) =>
    (f ~> g) ->
    (t f ~> t g)

type Bifunctor2Type :: Type
type Bifunctor2Type = FunctorType -> FunctorType -> FunctorType

type Bifunctor2 :: Bifunctor2Type -> Constraint
class
  (forall f. (Functor f) => Functor2 (t f)) =>
  Bifunctor2 t
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (t f g ~> t h i)
