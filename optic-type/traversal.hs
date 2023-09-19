module Traversal where

import Data.Foldable
import Data.Functor.Contravariant

type Fold s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

folding' :: Foldable g => Fold (g a) a
folding' = \afb -> \ga ->
  let fb = traverse_ afb ga
   in fb $< ()


type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

traversing :: Traversable g => Traversal (g a) (g b) a b
traversing = \afb -> \ga ->
  let fgb = traverse afb ga
   in fgb

traversing' :: Traversable g => Traversal (g a) (g b) a b
traversing' = traverse

setting :: Traversable g => Traversal (g a) (g b) a b
setting = \afb -> \ga ->
  let fgb = traverse afb ga
   in fgb


both :: Traversal (a, a) (b, b) a b
both = \afb -> \(a1, a2) ->
  let fb1 = afb a1
      fb2 = afb a2
      t = (,) <$> fb1 <*> fb2
   in t
