module Profunctor.DinaturalTransformation where

import Profunctor.Star

-- lmap f . alphaB . rmap f = rmap f . alphaA . lmap f

pureStar :: forall a f. (Applicative f) => (a -> a) -> Star f a a
pureStar f = Star g
  where
    g :: a -> f a
    g = pure . f

-- p :: b -> a
-- f :: a -> b
--
-- rmap f p = f . p :: b -> b
-- (pureStar . rmap f) p = Star (pure . f . p) :: Star f b b
-- (lmap f . pureStar . rmap f) p = Star (pure . f . p . f) :: Star f a b
--
-- lmap f p = p . f :: a -> a
-- (pureStar . lmap f) p = Star (pure . p . f) :: Star f a a
-- (rmap f . pureStar . lmap f) p = Star (fmap f . pure . p . f) :: Star f a b
--                                = Star (pure . f . p . f) :: Star f a b -- fmap f . pure - pure . f

hoistStar :: forall f g a. (forall x. f x -> g x) -> Star f a a -> Star g a a
hoistStar fx2gx (Star a2fa) = Star (fx2gx . a2fa)

-- p :: Star f b a -- b -> f a
-- f :: a -> b
--
-- rmap f p = fmap f . p :: b -> f b
-- (hoistStar . rmap f) p = fx2gx . fmap f . p :: b -> g b
-- (lmap f . hoistStar . rmap f) p = fx2gx . fmap f . p . f :: a -> g b
--
-- lmap f p = p . f :: a -> f a
-- (hoistStar . lmap f) p = fx2gx . p . f :: a -> g a
-- (rmap f . hoistStar . lmap f) p = fmap f . fx2gx . p . f :: a -> g b
--                                 = fx2gx . fmap f . p . f :: a -> g b -- fmap f . fx2gx = fx2gx . fmap f
