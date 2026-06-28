module ChurchFunctor where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Prelude hiding (Bool (..))

type Bool = forall r. (Sum (Const ()) (Const ()) r -> r) -> r

true, false :: Bool
true = \alg -> alg (InL (Const ()))
false = \alg -> alg (InR (Const ()))

type Natural = forall r. (Sum (Const ()) Identity r -> r) -> r

zero, one, two, three :: Natural
zero = \alg -> alg (InL (Const ()))
one = \alg -> alg (InR (Identity (alg (InL (Const ())))))
two = \alg -> alg (InR (Identity (alg (InR (Identity (alg (InL (Const ()))))))))
three = \alg -> alg (InR (Identity (alg (InR (Identity (alg (InR (Identity (alg (InL (Const ())))))))))))

succ :: Natural -> Natural
succ n = \alg -> alg (InR (Identity (n alg)))

type Pair a b = forall r. (Product (Const a) (Const b) r -> r) -> r

pair :: a -> b -> Pair a b
pair a b = \alg -> alg (Pair (Const a) (Const b))

fst :: Pair a b -> a
fst p = p (\(Pair (Const a) (Const b)) -> a)

snd :: Pair a b -> b
snd p = p (\(Pair (Const a) (Const b)) -> b)

type List a = forall r. (Sum (Const ()) (Product (Const a) Identity) r -> r) -> r

nil :: List a
nil = \alg -> alg (InL (Const ()))

cons :: a -> List a -> List a
cons a as = \alg -> alg (InR (Pair (Const a) (Identity (as alg))))

fold :: (a -> r -> r) -> r -> List a -> r
fold f a as = as g
  where
    g (InL (Const ())) = a
    g (InR (Pair (Const a') (Identity as'))) = f a' as'
