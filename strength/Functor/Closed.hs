module Functor.Closed where

import Data.Distributive

class (Functor f) => ClosedFunctor f where
  closed :: (a -> f b) -> f (a -> b)

instance ClosedFunctor ((->) r) where
  closed :: (a -> (r -> b)) -> (r -> (a -> b))
  closed a2fb = \r a ->
    let fb = a2fb a
     in fb r

instance (Functor f, Distributive f) => ClosedFunctor f where
  closed :: (a -> f b) -> f (a -> b)
  closed a2fb = distribute a2fb

unclosed :: (Functor f) => f (a -> b) -> (a -> f b)
unclosed fa2b = \a -> fmap (\a2b -> a2b a) fa2b
