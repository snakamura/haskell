module FixNat where

import Data.Kind
import Numeric.Natural
import Prelude hiding (Maybe (..))

import Fix
import Maybe

type Nat :: Type
type Nat = Fix Maybe

zero, one, two :: Nat
zero = Fix Nothing
one = Fix (Just (Fix Nothing))
two = Fix (Just (Fix (Just (Fix Nothing))))

fromNatural :: Natural -> Nat
fromNatural = ana coalg
  where
    coalg :: Natural -> Maybe Natural
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = Fix (Just inf)

toNatural :: Nat -> Natural
toNatural = cata alg
  where
    alg :: Maybe Natural -> Natural
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Natural -> Nat -> Bool
is n nat = cata alg nat n
  where
    alg :: Maybe (Natural -> Bool) -> (Natural -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)

type Fix' :: (Type -> Type) -> Type
newtype Fix' f = Fix' (() -> f (Fix' f))

type Nat' :: Type
type Nat' = Fix' Maybe

inf' :: Nat'
inf' = Fix' (\_ -> Just inf')
