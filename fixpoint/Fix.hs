module Fix where

import Data.Kind

type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix (f (Fix f))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . project

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = embed . fmap (ana coalg) . coalg

embed :: f (Fix f) -> Fix f
embed = Fix

project :: Fix f -> f (Fix f)
project (Fix ffixf) = ffixf
