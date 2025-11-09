module Functor.ProductStrong where

class (Functor f) => ProductStrongFunctor f where
  strength :: (a, f b) -> f (a, b)

instance ProductStrongFunctor Maybe where
  strength :: (a, Maybe b) -> Maybe (a, b)
  strength (a, Just b) = Just (a, b)
  strength (_, Nothing) = Nothing

instance (Functor f) => ProductStrongFunctor f where
  strength :: (a, f b) -> f (a, b)
  strength (a, fb) = fmap (\b -> (a, b)) fb
