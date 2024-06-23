module MonoidalFunctor where

import Control.Applicative
import Coproduct ()
import Data.Void
import Monoid
import Product ()

instance MonoidalFunctor (,) (,) Maybe where
  nu :: (Maybe a, Maybe b) -> Maybe (a, b)
  nu (Just a, Just b) = Just (a, b)
  nu _ = Nothing

  unit :: () -> Maybe ()
  unit () = Just ()

instance MonoidalFunctor (,) (,) [] where
  nu :: ([a], [b]) -> [(a, b)]
  nu (as, bs) = [(a, b) | a <- as, b <- bs]

  unit :: () -> [()]
  unit () = [()]

instance
  ( MonoidalFunctor (,) (,) f,
    Functor f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> f a -> f b
  fab <*> fa = (\(ab, a) -> ab a) <$> nu (fab, fa)

  pure :: a -> f a
  pure a = a <$ unit @(,) @(,) ()

instance MonoidalFunctor Either (,) Maybe where
  nu :: (Maybe a, Maybe b) -> Maybe (Either a b)
  nu (Just a, _) = Just $ Left a
  nu (_, Just b) = Just $ Right b
  nu _ = Nothing

  unit :: () -> Maybe Void
  unit _ = Nothing

instance MonoidalFunctor Either (,) [] where
  nu :: ([a], [b]) -> [Either a b]
  nu (as, bs) = (Left <$> as) <> (Right <$> bs)

  unit :: () -> [Void]
  unit _ = []

instance
  ( MonoidalFunctor Either (,) f,
    Functor f,
    Applicative f
  ) =>
  Alternative f
  where
  (<|>) :: f a -> f a -> f a
  fa1 <|> fa2 = either id id <$> nu (fa1, fa2)

  empty :: f a
  empty = absurd <$> unit @Either @(,) ()
