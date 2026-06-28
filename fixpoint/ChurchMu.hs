module ChurchMu where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Prelude hiding (Bool (..))

import Mu

type Bool = Mu (Sum (Const ()) (Const ()))

true, false :: Bool
true = In $ \alg -> alg (InL (Const ()))
false = In $ \alg -> alg (InR (Const ()))

type Natural = Mu (Sum (Const ()) Identity)

zero, one, two, three :: Natural
zero = In $ \alg -> alg (InL (Const ()))
one = In $ \alg -> alg (InR (Identity (alg (InL (Const ())))))
two = In $ \alg -> alg (InR (Identity (alg (InR (Identity (alg (InL (Const ()))))))))
three = In $ \alg -> alg (InR (Identity (alg (InR (Identity (alg (InR (Identity (alg (InL (Const ())))))))))))

succ :: Natural -> Natural
succ (In n) = In $ \alg -> alg (InR (Identity (n alg)))

type Pair a b = Mu (Product (Const a) (Const b))

pair :: a -> b -> Pair a b
pair a b = In $ \alg -> alg (Pair (Const a) (Const b))

fst :: Pair a b -> a
fst (In p) = p (\(Pair (Const a) (Const b)) -> a)

snd :: Pair a b -> b
snd (In p) = p (\(Pair (Const a) (Const b)) -> b)

type List a = Mu (Sum (Const ()) (Product (Const a) Identity))

nil :: List a
nil = In $ \alg -> alg (InL (Const ()))

cons :: a -> List a -> List a
cons a (In as) = In $ \alg -> alg (InR (Pair (Const a) (Identity (as alg))))

fold :: (a -> r -> r) -> r -> List a -> r
fold f a (In as) = as g
  where
    g (InL (Const ())) = a
    g (InR (Pair (Const a') (Identity as'))) = f a' as'
