module NuNat where

import Data.Kind
import Numeric.Natural
import Prelude hiding (Maybe (..))

import Maybe
import Nu

type Nat :: Type
type Nat = Nu Maybe

projectNat :: Nat -> Maybe Nat
projectNat (Out next state) =
  case next state of
    Just state' -> Just $ Out next state'
    Nothing -> Nothing

zero, one :: Nat
zero = Out (\() -> Nothing) ()
one = Out f True
  where
    f False = Nothing
    f True = Just False

fromNatural :: Natural -> Nat
fromNatural = ana coalg
  where
    coalg :: Natural -> Maybe Natural
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = ana (\_ -> Just ()) ()

toNatural :: Nat -> Natural
toNatural = cata alg
  where
    alg :: Maybe Natural -> Natural
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Natural -> Nat -> Bool
is n nat = case (project nat, n) of
    (Nothing, 0) -> True
    (Nothing, _) -> False
    (Just nat', n') -> n' > 0 && is (n' - 1) nat'

is' :: Natural -> Nat -> Bool
is' n nat = cata alg nat n
 where
    alg :: Maybe (Natural -> Bool) -> (Natural -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
