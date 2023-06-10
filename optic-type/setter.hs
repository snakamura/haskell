module Setter where

import Data.Functor.Identity
import Data.Kind

type Setter1 s t a b = (a -> Identity b) -> (s -> Identity t)

setting1 :: ((a -> b) -> (s -> t)) -> Setter1 s t a b
setting1 map = \afb -> \s ->
  let ab a = runIdentity (afb a)
      st = map ab
      t = st s
   in Identity t

over1 :: Setter1 s t a b -> (a -> b) -> s -> t
over1 setter ab s =
  let afb a = Identity $ ab a
      sft = setter afb
      Identity t = sft s
   in t

type Identical2 :: (Type -> Type) -> Constraint
class Identical2 f where
  extract2 :: f a -> a

type Setter2 s t a b = forall f. (Functor f, Applicative f, Identical2 f) => (a -> f b) -> (s -> f t)

setting2 :: ((a -> b) -> (s -> t)) -> Setter2 s t a b
setting2 map = \afb -> \s ->
  let ab a = extract2 (afb a)
      st = map ab
      t = st s
   in pure t

instance Identical2 Identity where
  extract2 = runIdentity

type Setting2 s t a b = (a -> Identity b) -> (s -> Identity t)

over2 :: Setting2 s t a b -> (a -> b) -> s -> t
over2 setter ab s =
  let afb a = Identity $ ab a
      sft = setter afb
      Identity t = sft s
   in t

identicalTraverse :: (Identical2 f, Applicative f, Functor g) => (a -> g b) -> f a -> g (f b)
identicalTraverse agb fa =
  let a = extract2 fa
      gb = agb a
      gfa = fmap pure gb
   in gfa

type Identical3 :: (Type -> Type) -> Constraint
class (Applicative f, Traversable f) => Identical3 f where
  extract3 :: f a -> a

type Setter3 s t a b = forall f. Identical3 f => (a -> f b) -> (s -> f t)

setting3 :: ((a -> b) -> (s -> t)) -> Setter3 s t a b
setting3 map = \afb -> \s ->
  let ab a = extract3 (afb a)
      st = map ab
      t = st s
   in pure t

instance Identical3 Identity where
  extract3 = runIdentity

type Identical :: (Type -> Type) -> Constraint
class (Applicative f, Traversable f) => Identical f where
  extract :: f a -> a

instance Identical Identity where
  extract = runIdentity

type Setter s t a b = forall f. Identical f => (a -> f b) -> (s -> f t)

type Setting s t a b = (a -> Identity b) -> (s -> Identity t)

setting :: ((a -> b) -> (s -> t)) -> Setter s t a b
setting map afb = pure . map (extract . afb)

over :: Setting s t a b -> (a -> b) -> s -> t
over setter ab = runIdentity . setter (Identity . ab)
