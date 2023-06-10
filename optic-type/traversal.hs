module Traversal where

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

both :: Traversal (a, a) (b, b) a b
both = \afb -> \(a1, a2) ->
  let fb1 = afb a1
      fb2 = afb a2
      t = (,) <$> fb1 <*> fb2
   in t
