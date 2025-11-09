module Profunctor.ProductStrong where

import Data.Profunctor (Profunctor (..))
import Data.Tuple
import Functor.ProductStrong
import Profunctor.Star

class (Profunctor p) => ProductStrongProfunctor p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)

instance ProductStrongProfunctor (->) where
  first :: (a -> b) -> ((a, c) -> (b, c))
  first a2b (a, c) = (a2b a, c)

  second :: (a -> b) -> ((c, a) -> (c, b))
  second a2b (c, a) = (c, a2b a)

instance (ProductStrongFunctor f) => ProductStrongProfunctor (Star f) where
  first :: Star f a b -> Star f (a, c) (b, c)
  first (Star a2fb) = Star (\ac -> fmap swap (strength (fmap a2fb (swap ac))))

  second :: Star f a b -> Star f (c, a) (c, b)
  second (Star a2fb) = Star (\ca -> strength $ fmap a2fb ca)

{-
instance (Functor f) => ProductStrongProfunctor (Star f) where
  first :: Star f a b -> Star f (a, c) (b, c)
  first (Star a2fb) =
    Star
      ( \(a, c) ->
          let fb = a2fb a
           in fmap (\b -> (b, c)) fb
      )

  second :: Star f a b -> Star f (c, a) (c, b)
  second (Star a2fb) =
    Star
      ( \(c, a) ->
          let fb = a2fb a
           in fmap (\b -> (c, b)) fb
      )
-}
