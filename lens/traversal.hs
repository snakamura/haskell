{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Monoid;

type LensLike' f a b = (b -> f b) -> a -> f a
type Lens' a b = Functor f => LensLike' f a b
type Traversal' a b = Applicative f => LensLike' f a b
type FoldLike' r a b = LensLike' (Constant r) a b
type Setter' a b = LensLike' Identity a b

--view :: Lens' a b -> a -> b
--view :: Monoid b => Traversal' a b -> a -> b
view :: FoldLike' b a b -> a -> b
--view :: ((b -> Constant b x) -> a -> Constant b y) -> a -> b
view l a = getConstant $ l Constant a

views :: FoldLike' r a b -> (b -> r) -> a -> r
views l f a = getConstant $ l (Constant . f) a

--set :: Lens' a b -> b -> a -> a
--set :: Traversal' a b -> b -> a -> a
set :: Setter' a b -> b -> a -> a
--set :: ((b -> Identity b) -> a -> Identity a) -> b -> a -> a
set l b a = runIdentity $ l (Identity . const b) a

--over :: Lens' a b -> (b -> b) -> a -> a
--over :: Traversal' a b -> (b -> b) -> a -> a
over :: Setter' a b -> (b -> b) -> a -> a
--over :: ((b -> Identity b) -> a -> Identity a) -> (b -> b) -> a -> a
over l f a = runIdentity $ l (Identity . f) a

_1 :: Lens' (p, q) p
_1 g (x, y) = (\x' -> (x', y)) <$> g x

both :: Traversal' (a, a) a
both g (x, y) = (,) <$> g x <*> g y

{-
view both (p, q) = getConstant $ both Constant (p, q)
                 -- both = \g (x, y) -> (,) <$> g x <*> g y
                 = getConstant $ (\g (x, y) -> (,) <$> g x <*> g y) Constant (p, q)
                 -- g = Constant, x = p, y = q
                 = getConstant $ (,) <$> Constant p <*> Constant q
                 -- _ <$> Constant p = p
                 = getConstant $ Constant p <*> Constant q
                 -- Constant p <*> Constant q = Constant (p <> q)
                 = getConstant $ Constant (p <> q)
                 = p <> q

over both f (p, q) = runIdentity $ both (Identity . f) (p, q)
                   -- both = \g (x, y) -> (,) <$> g x <*> g y
                   = runIdentity $ (\g (x, y) -> (,) <$> g x <*> g y) (Identity . f) (p, q)
                   -- g = Identity, x = p, y = q
                   = runIdentity $ (,) <$> (Identity . f) p <*> (Identity . f) q
                   = runIdentity $ (,) <$> Identity (f p) <*> Identity (f q)
                   = runIdentity $ (\x' y' -> (x', y')) <$> Identity (f p) <*> Identity (f q)
                   -- f <$> Identity p = Identity (f p)
                   = runIdentity $ Identity ((\x' y' -> (x', y')) (f p)) <*> Identity (f q)
                   = runIdentity $ Identity (\y' -> (f p, y')) <*> Identity (f q)
                   -- Identity f <*> Identity p = Identity (f p)
                   = runIdentity $ Identity ((\y' -> (f p, y')) (f q))
                   = runIdentity $ Identity (f p, f q)
                   = (f p, f q)
-}
