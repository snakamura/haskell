module Profunctor.Closed where

import Data.Distributive
import Data.Profunctor (Profunctor (..))
-- import Functor.Closed
import Profunctor.Costar
import Profunctor.Star

class (Profunctor p) => ClosedProfunctor p where
  closed :: p a b -> p (c -> a) (c -> b)

instance ClosedProfunctor (->) where
  closed :: (a -> b) -> ((c -> a) -> (c -> b))
  closed a2b c2a c =
    let a = c2a c
        b = a2b a
     in b

{-
instance (ClosedFunctor f) => ClosedProfunctor (Star f) where
  closed :: forall a b c. Star f a b -> Star f (c -> a) (c -> b)
  closed (Star a2fb) = Star g
    where
      g :: (c -> a) -> f (c -> b)
      g c2a = Functor.Closed.closed h
        where
          h = undefined
-}

instance (Distributive f) => ClosedProfunctor (Star f) where
  closed :: forall a b c. Star f a b -> Star f (c -> a) (c -> b)
  closed (Star a2fb) = Star g
    where
      g :: (c -> a) -> f (c -> b)
      g c2a = distribute h
        where
          h :: c -> f b
          h c = a2fb (c2a c)

instance (Functor f) => ClosedProfunctor (Costar f) where
  closed :: forall a b c. Costar f a b -> Costar f (c -> a) (c -> b)
  closed (Costar fa2b) = Costar g
    where
      g :: f (c -> a) -> (c -> b)
      g fc2a c = fa2b (fmap h fc2a)
        where
          h :: (c -> a) -> a
          h c2a = c2a c
