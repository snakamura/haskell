module Fix where

import Data.Kind

type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix (f (Fix f))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg (Fix ffixf) =
  let fa = cata alg <$> ffixf
   in alg fa

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg a =
  let fa = coalg a
   in Fix $ ana coalg <$> fa
