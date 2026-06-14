module MuNat where

import Data.Function
import Data.Kind
import Numeric.Natural
import Prelude hiding (Maybe (..))

import Maybe
import Mu

type Nat :: Type
type Nat = Mu Maybe

zero, one, two :: Nat
zero = In (\alg -> alg Nothing)
one = In (\alg -> alg (Just (alg Nothing)))
two = In (\alg -> alg (Just (alg (Just (alg Nothing)))))

fromNatural :: Natural -> Nat
fromNatural = ana coalg
  where
    coalg :: Natural -> Maybe Natural
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = In (\alg -> fix (alg . Just))

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
