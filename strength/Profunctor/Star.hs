module Profunctor.Star where

import Data.Profunctor (Profunctor (..))

newtype Star f a b = Star (a -> f b)

instance (Functor f) => Profunctor (Star f) where
  dimap :: (s -> a) -> (b -> t) -> (Star f a b -> Star f s t)
  dimap s2a b2t (Star a2fb) =
    Star
      ( \s ->
          let a = s2a s
              fb = a2fb a
              ft = fmap b2t fb
           in ft
      )
