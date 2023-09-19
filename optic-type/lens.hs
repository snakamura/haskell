module Lens where

type Lens1 s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

lens1 :: (s -> a) -> ((a -> b) -> (s -> t)) -> Lens1 s t a b
lens1 get map = \afb -> \s ->
  let a = get s
      fb = afb a
      ft = fmap (\b -> let ab _ = b
                           st = map ab
                           t = st s
                        in t) fb
   in ft

lens1' :: (s -> a) -> (b -> s -> t) -> Lens1 s t a b
lens1' get set = \afb -> \s ->
  let a = get s
      fb = afb a
      ft = fmap (\b -> let st = set b
                           t = st s
                        in t) fb
   in ft

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \afb -> \s ->
  let a = get s
      fb = afb a
   in fmap (set s) fb

lens' :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens' get set afb s = fmap (set s) (afb (get s))

_1 :: Lens (a, b) (a', b) a a'
_1 = lens get set
  where
    get (x, _) = x
    set (_, y) x = (x, y)
