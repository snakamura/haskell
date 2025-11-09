module Profunctor.Costar where

import Data.Profunctor (Profunctor (..))

newtype Costar f a b = Costar (f a -> b)

instance (Functor f) => Profunctor (Costar f) where
  dimap :: (s -> a) -> (b -> t) -> (Costar f a b -> Costar f s t)
  dimap s2a b2t (Costar fa2b) =
    Costar
      ( \fs ->
          let fa = fmap s2a fs
              b = fa2b fa
              t = b2t b
           in t
      )
