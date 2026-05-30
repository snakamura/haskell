module FixNat where

import Data.Kind
import Prelude hiding (Maybe (..))

import Fix
import Maybe

type Nat :: Type
type Nat = Fix Maybe

zero, one, two :: Nat
zero = Fix Nothing
one = Fix (Just (Fix Nothing))
two = Fix (Just (Fix (Just (Fix Nothing))))

fromInt :: Int -> Nat
fromInt = ana coalg
  where
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = Fix (Just inf)

toInt :: Nat -> Int
toInt = cata alg
  where
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Int -> Nat -> Bool
is 0 (Fix Nothing) = True
is 0 _ = False
is n (Fix (Just mu)) = is (n - 1) mu
is _ _ = False
