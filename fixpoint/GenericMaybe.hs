module GenericMaybe where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Kind
import Prelude hiding (Maybe(..))

type Maybe = Sum (Const ()) Identity

type Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (Mu fmuf) =
  let fa = cata alg <$> fmuf
   in alg fa

ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a =
  let fa = coalg a
   in Mu $ ana coalg <$> fa

type Nat = Mu Maybe

zero, one, two :: Nat
zero = Mu (InL (Const ()))
one = Mu (InR (Identity (Mu (InL (Const ())))))
two = Mu (InR (Identity (Mu (InR (Identity (Mu (InL (Const ()))))))))

fromInt :: Int -> Nat
fromInt n = ana coalg n
  where
    coalg 0 = InL (Const ())
    coalg n' = InR (Identity (n' - 1))

inf :: Nat
inf = Mu (InR (Identity inf))

toInt :: Nat -> Int
toInt = cata alg
  where
    alg (InL (Const ())) = 0
    alg (InR (Identity n)) = n + 1

is :: Int -> Nat -> Bool
is 0 (Mu (InL (Const ()))) = True
is 0 _ = False
is n (Mu (InR (Identity nat))) = is (n - 1) nat
is _ _ = False

type Nu :: (Type -> Type) -> Type
data Nu f where
  Nu :: (s -> f s) -> s -> Nu f

cata' :: (Functor f) => (f a -> a) -> Nu f -> a
cata' alg nu =
  let fnuf = project nu
   in alg $ cata' alg <$> fnuf

ana' :: (Functor f) => (a -> f a) -> a -> Nu f
ana' = Nu

project :: Functor f => Nu f -> f (Nu f)
project (Nu next state) = Nu next <$> next state

type CoNat = Nu Maybe

zero', one' :: CoNat
zero' = Nu (\() -> InL (Const ())) ()
one' = Nu f False
  where
    f False = InR (Identity True)
    f True = InL (Const ())

fromInt' :: Int -> CoNat
fromInt' = ana' coalg
  where
    coalg 0 = InL (Const ())
    coalg n = InR (Identity (n - 1))

inf' :: CoNat
inf' = Nu (\() -> InR (Identity ())) ()

toInt' :: CoNat -> Int
toInt' = cata' alg
  where
    alg (InL (Const ())) = 0
    alg (InR (Identity n)) = n + 1

is' :: Int -> CoNat -> Bool
is' n coNat =
  case (project coNat, n) of
    (InR (Identity _), 0) -> False
    (InR (Identity coNat'), n') -> is' (n' - 1) coNat'
    (InL (Const ()), 0) -> True
    (InL (Const ()), _) -> False
