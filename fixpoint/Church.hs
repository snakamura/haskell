module Church where

import Prelude hiding (Bool (..))

type Bool = forall r. r -> r -> r

true, false :: Bool
true = \t f -> t
false = \t f -> f

type Natural = forall r. (r -> r) -> r -> r

zero, one, two, three :: Natural
zero = \f z -> z
one = \f z -> f z
two = \f z -> f (f z)
three = \f z -> f (f (f z))

succ :: Natural -> Natural
succ n = \f z -> f (n f z)

type Pair a b = forall r. (a -> b -> r) -> r

pair :: a -> b -> Pair a b
pair a b = \f -> f a b

fst :: Pair a b -> a
fst p = p (\a b -> a)

snd :: Pair a b -> b
snd p = p (\a b -> b)

type List a = forall r. (a -> r -> r) -> r -> r

nil :: List a
nil = \f r -> r

cons :: a -> List a -> List a
cons a as = \f r -> f a (as f r)

fold :: (a -> r -> r) -> r -> List a -> r
fold f a as = as f a
