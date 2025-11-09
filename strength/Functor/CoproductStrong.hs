module Functor.CoproductStrong where

class (Functor f) => CoproductStrongFunctor f where
  strength :: Either a (f b) -> f (Either a b)

instance CoproductStrongFunctor Maybe where
  strength :: Either a (Maybe b) -> Maybe (Either a b)
  strength (Left a) = Just (Left a)
  strength (Right fb) = fmap Right fb

instance (Functor f, Applicative f) => CoproductStrongFunctor f where
  strength :: Either a (f b) -> f (Either a b)
  strength (Left a) = pure (Left a)
  strength (Right fb) = fmap Right fb
