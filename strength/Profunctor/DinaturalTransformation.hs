module Profunctor.DinaturalTransformation where

import Profunctor.Star

-- lmap f . alphaB . rmap f = rmap f . alphaA . lmap f

arrowToStar :: forall a f. (Applicative f) => (a -> a) -> Star f a a
arrowToStar f = Star g
  where
    g :: a -> f a
    g = pure . f

-- h :: b -> a -- p b a
-- f :: a -> b
--
-- rmap f h = f . h
-- (arrowToStar . rmap f) h = Star (pure . f . h)
-- (lmap f . arrowToStar . rmap f) h = Star (pure . f . h . f)

-- lmap f h = h . f
-- (arrowToStar . lmap f) h = Star (pure . h . f)
-- (rmap f . arrowToStar . lmap f) h = Star (fmap f . pure . h . f) = Star (pure . f . h . f)
