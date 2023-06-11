module Lens where

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \afb -> \s ->
  let a = get s
      fb = afb a
   in fmap (set s) fb

_1 :: Lens (a, b) (a', b) a a'
_1 = lens get set
  where
    get (x, _) = x
    set (_, y) x = (x, y)
