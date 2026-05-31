module Fix where

import Data.Kind

type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix (f (Fix f))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg (Fix ffixf) = alg $ cata alg <$> ffixf

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg a = Fix $ ana coalg <$> coalg a

embed :: f (Fix f) -> Fix f
embed = Fix

project :: Fix f -> f (Fix f)
project (Fix ffixf) = ffixf
