module FunctorCompose where

import Control.Applicative qualified
import Control.Monad qualified
import Data.Functor qualified
import Data.Maybe
import Functor
import FunctorMonoid
import NaturalTransformation
import Prelude (($), (.))

type Compose :: FunctorType -> FunctorType -> FunctorType
newtype Compose f g a = Compose {getCompose :: (f (g a))}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> (Compose f g a -> Compose f g b)
  fmap ab (Compose fga) = Compose (fmap (fmap ab) fga)

instance (Functor f) => NaturalTransformation (Compose f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Compose f g ~> Compose f h)
  ntmap gh (Compose fga) = Compose (fmap gh fga)

instance
  (forall f. (Functor f) => NaturalTransformation (Compose f)) =>
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

instance Functor Identity where
  fmap :: (a -> b) -> (Identity a -> Identity b)
  fmap ab (Identity a) = Identity (ab a)

-- (Hask -> Hask, Compose, Identity) is a monoidal category

instance FunctorMonoidalCategory Compose where
  type FunctorUnit Compose = Identity

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

instance FunctorMonoidObject Compose Identity where
  mu :: Compose Identity Identity ~> Identity
  mu (Compose (Identity (Identity a))) = Identity a

  eta :: Identity ~> Identity
  eta (Identity a) = Identity a

instance Functor Maybe where
  fmap :: (a -> b) -> (Maybe a -> Maybe b)
  fmap ab (Just a) = Just (ab a)
  fmap _ Nothing = Nothing

instance FunctorMonoidObject Compose Maybe where
  mu :: Compose Maybe Maybe ~> Maybe
  mu (Compose (Just (Just a))) = Just a
  mu (Compose _) = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance
  {-# OVERLAPPABLE #-}
  ( Data.Functor.Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Control.Applicative.Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa =
    mu $
      Compose $
        fmap
          ( \ab ->
              mu $ Compose $ fmap (Control.Applicative.pure . ab) fa
          )
          fab

  pure :: a -> f a
  pure = eta @Compose . Identity

instance
  {-# OVERLAPPABLE #-}
  ( Data.Functor.Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Control.Monad.Monad f
  where
  (>>=) :: f a -> (a -> f b) -> f b
  (>>=) fa afb = mu $ Compose $ fmap afb fa
