module Day where

import Data.Maybe
import Functor
import FunctorMonoid
import NaturalTransformation
import Prelude (const, flip)

type Day :: FunctorType -> FunctorType -> FunctorType
data Day f g a = forall p q. Day (f p) (g q) (p -> q -> a)

instance Functor (Day f g) where
  fmap :: (a -> b) -> (Day f g a -> Day f g b)
  fmap ab (Day f g pqa) = Day f g (\p q -> ab (pqa p q))

instance (Functor h) => NaturalTransformation (Day h) where
  ntmap ::
    (Functor f, Functor g) =>
    (f ~> g) ->
    (Day h f ~> Day h g)
  ntmap fg (Day h f pqa) = Day h (fg f) pqa

instance
  (forall f. NaturalTransformation (Day f)) =>
  BinaturalTransformation Day
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Day f g ~> Day h i)
  bintmap fh gi (Day f g pqa) = Day (fh f) (gi g) pqa

newtype Identity a = Identity a

-- (Hask -> Hask, Day, Identity) is a monoidal category

assoc ::
  (Functor f, Functor g, Functor h) =>
  Day f (Day g h) ~> Day (Day f g) h
assoc (Day fp (Day gq hr qrs) psa) =
  Day (Day fp gq (,)) hr (\(p, q) r -> psa p (qrs q r))

assocInv ::
  (Functor f, Functor g, Functor h) =>
  Day (Day f g) h ~> Day f (Day g h)
assocInv (Day (Day fp gq pqs) hr sra) =
  Day fp (Day gq hr (,)) (\p (q, r) -> sra (pqs p q) r)

left :: (Functor f) => Day Identity f ~> f
left (Day (Identity p) fq pqa) = fmap (pqa p) fq

leftInv :: (Functor f) => f ~> Day Identity f
leftInv fp = Day (Identity ()) fp (flip const)

right :: (Functor f) => Day f Identity ~> f
right (Day fp (Identity q) pqa) = fmap (flip pqa q) fp

rightInv :: (Functor f) => f ~> Day f Identity
rightInv fa = Day fa (Identity ()) const

instance FunctorMonoid Maybe where
  type Tensor Maybe = Day Maybe Maybe
  type Id Maybe = Identity

  mu :: Day Maybe Maybe ~> Maybe
  mu (Day (Just p) (Just q) pqa) = Just (pqa p q)
  mu _ = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a
