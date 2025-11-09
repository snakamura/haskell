module Functor.Closed where

import Data.Distributive

class (Functor f) => ClosedFunctor f where
  closed :: f (a -> b) -> (a -> f b)

instance ClosedFunctor Maybe where
  closed :: Maybe (a -> b) -> (a -> Maybe b)
  closed (Just a2b) = \a -> Just (a2b a)
  closed Nothing = \_ -> Nothing

instance (Functor f, Distributive f) => ClosedFunctor f where
  closed :: f (((->) a) b) -> (->) a (f b)
  closed fab = distribute fab
