module Profunctor.Pure where

import Data.Profunctor (Profunctor (..))

newtype Pure a b = Pure (a -> b)

instance Profunctor Pure where
  dimap :: (s -> a) -> (b -> t) -> Pure a b -> Pure s t
  dimap s2a b2t (Pure f) = Pure (b2t . f . s2a)
