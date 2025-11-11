module Profunctor.DinaturalTransformation where

import Profunctor.Costar
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
-- (hoistStar fx2gx . rmap f) p = fx2gx . fmap f . p :: b -> g b
-- (lmap f . hoistStar fx2gx . rmap f) p = fx2gx . fmap f . p . f :: a -> g b
--
-- lmap f p = p . f :: a -> f a
-- (hoistStar fx2gx . lmap f) p = fx2gx . p . f :: a -> g a
-- (rmap f . hoistStar fx2gx . lmap f) p = fmap f . fx2gx . p . f :: a -> g b
      --                                 = fx2gx . fmap f . p . f :: a -> g b -- fmap f . fx2gx = fx2gx . fmap f

hoistCostar :: forall f g a. (forall x. g x -> f x) -> Costar f a a -> Costar g a a
hoistCostar gx2fx (Costar fa2a) = Costar (fa2a . gx2fx)

-- p :: Costar f b a -- f b -> a
-- f :: a -> b
--
-- rmap f p = f . p :: f b -> b
-- (hoistCostar gx2fx . rmap f) p = f . p . gx2fx :: g b -> b
-- (lmap f . hoistCostar gx2fx . rmap f) p = f . p . gx2fx . fmap f :: g a -> b
--
-- lmap f p = p . fmap f :: f a -> a
-- (hoistCostar gx2fx . lmap f) p = p . fmap f . gx2fx :: g a -> a
-- (rmap f . hoistCostar gx2fx . lmap f) p = f . p . fmap f . gx2fx :: g a -> b
--                                         = f . p . gx2fx . fmap f :: g a -> b -- fmap f . gx2fx = gx2fx . fmap f
