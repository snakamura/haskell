module Nu where

import Data.Kind

type Nu :: (Type -> Type) -> Type
data Nu f where
  Nu :: (s -> f s) -> s -> Nu f

cata :: (Functor f) => (f a -> a) -> Nu f -> a
cata alg nu =
  let fnuf = project nu
   in alg $ cata alg <$> fnuf

ana :: (Functor f) => (a -> f a) -> a -> Nu f
ana = Nu

project :: (Functor f) => Nu f -> f (Nu f)
project (Nu next state) = Nu next <$> next state
