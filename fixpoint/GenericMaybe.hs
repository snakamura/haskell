module GenericMaybe where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Kind
import Prelude hiding (Maybe(..))

type Maybe = Sum (Const ()) Identity

type Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

type Nat = Mu Maybe

zero, one, two :: Nat
zero = Mu (InL (Const ()))
one = Mu (InR (Identity (Mu (InL (Const ())))))
two = Mu (InR (Identity (Mu (InR (Identity (Mu (InL (Const ()))))))))

fromInt :: Int -> Nat
fromInt 0 = Mu $ InL $ Const ()
fromInt n = Mu $ InR $ Identity $ fromInt $ n - 1

inf :: Nat
inf = Mu (InR (Identity inf))

is :: Int -> Nat -> Bool
is 0 (Mu (InL (Const ()))) = True
is 0 _ = False
is n (Mu (InR (Identity nat))) = is (n - 1) nat
is _ _ = False

type Nu :: (Type -> Type) -> Type
data Nu f where
  Nu :: (s -> f s) -> s -> Nu f

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
fromInt' 0 = Nu (\() -> InL (Const ())) ()
fromInt' n = Nu f n
  where
    f 0 = InL (Const ())
    f n' = InR (Identity (n' - 1))

inf' :: CoNat
inf' = Nu (\() -> InR (Identity ())) ()

is' :: Int -> CoNat -> Bool
is' n coNat =
  case (project coNat, n) of
    (InR (Identity _), 0) -> False
    (InR (Identity coNat'), n') -> is' (n' - 1) coNat'
    (InL (Const ()), 0) -> True
    (InL (Const ()), _) -> False
