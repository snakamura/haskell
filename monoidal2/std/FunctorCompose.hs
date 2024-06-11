module FunctorCompose where

import Data.Functor.Compose
import Data.Functor.Identity
import FunctorMonoid
import Functor2

instance (Functor f) => Functor2 (Compose f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Compose f g ~> Compose f h)
  ntmap gh (Compose fga) = Compose (fmap gh fga)

instance
  (forall f. (Functor f) => Functor2 (Compose f)) =>
  Bifunctor2 Compose
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Compose f g ~> Compose h i)
  bintmap fh gi (Compose fga) =
    let fia = fmap gi fga
     in Compose (fh fia)

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

instance FunctorMonoidObject Compose Maybe where
  mu :: Compose Maybe Maybe ~> Maybe
  mu (Compose (Just (Just a))) = Just a
  mu (Compose _) = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa =
    mu $
      Compose $
        fmap
          ( \ab ->
              mu $ Compose $ fmap (pure . ab) fa
          )
          fab

  pure :: a -> f a
  pure = eta @Compose . Identity

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Compose f
  ) =>
  Monad f
  where
  (>>=) :: f a -> (a -> f b) -> f b
  (>>=) fa afb = mu $ Compose $ fmap afb fa