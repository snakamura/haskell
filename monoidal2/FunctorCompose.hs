module FunctorCompose where

import Data.Maybe
import Functor
import FunctorMonoid
import NaturalTransformation
import Prelude ()

type Compose :: FunctorType -> FunctorType -> FunctorType
newtype Compose f g a = Compose {getCompose :: (f (g a))}

--instance (Functor f, Functor g) => Functor (Compose f g) where
--  fmap :: (a -> b) -> (Compose f g a -> Compose f g b)
--  fmap ab (Compose fga) = Compose (fmap (fmap ab) fga)

instance (Functor h) => NaturalTransformation (Compose h) where
  ntmap ::
    (Functor f, Functor g) =>
    (f ~> g) ->
    (Compose h f ~> Compose h g)
  ntmap fg (Compose hfa) = Compose (fmap fg hfa)

instance
  (forall f. NaturalTransformation (Compose f)) =>
  BinaturalTransformation Compose
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Compose f g ~> Compose h i)
  bintmap fh gi (Compose fga) =
    let fia = fmap gi fga
     in Compose (fh fia)

newtype Identity a = Identity a

-- (Hask -> Hask, Compose, Identity) is a monoidal category

assoc ::
  (Functor f, Functor g, Functor h) =>
  Compose f (Compose g h) ~> Compose (Compose f g) h
assoc (Compose fcgha) =
  let fgha = fmap (\(Compose gha) -> gha) fcgha
   in Compose (Compose fgha)

assocInv ::
  (Functor f, Functor g, Functor h) =>
  Compose (Compose f g) h ~> Compose f (Compose g h)
assocInv (Compose (Compose fgh)) = Compose (fmap Compose fgh)

left :: (Functor f) => Compose Identity f ~> f
left (Compose (Identity f)) = f

leftInv :: f ~> Compose Identity f
leftInv f = Compose (Identity f)

right :: (Functor f) => Compose f Identity ~> f
right (Compose fia) = fmap (\(Identity a) -> a) fia

rightInv :: (Functor f) => f ~> Compose f Identity
rightInv fa = Compose (fmap Identity fa)

instance FunctorMonoid Identity where
  type Tensor Identity = Compose Identity Identity
  type Id Identity = Identity

  mu :: Compose Identity Identity ~> Identity
  mu (Compose (Identity (Identity a))) = Identity a

  eta :: Identity ~> Identity
  eta (Identity a) = Identity a

instance FunctorMonoid Maybe where
  type Tensor Maybe = Compose Maybe Maybe
  type Id Maybe = Identity

  mu :: Compose Maybe Maybe ~> Maybe
  mu (Compose (Just (Just a))) = Just a
  mu (Compose _) = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a
