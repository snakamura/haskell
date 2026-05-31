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
    coalg :: Int -> Maybe Int
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = Fix (Just inf)

toInt :: Nat -> Int
toInt = cata alg
  where
    alg :: Maybe Int -> Int
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Int -> Nat -> Bool
is n nat = cata alg nat n
  where
    alg :: Maybe (Int -> Bool) -> (Int -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
