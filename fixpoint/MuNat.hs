module MuNat where

import Data.Function
import Data.Kind
import Prelude hiding (Maybe (..))

import Maybe
import Mu

type Nat :: Type
type Nat = Mu Maybe

zero, one, two :: Nat
zero = Mu (\alg -> alg Nothing)
one = Mu (\alg -> alg (Just (alg Nothing)))
two = Mu (\alg -> alg (Just (alg (Just (alg Nothing)))))

fromInt :: Int -> Nat
fromInt = ana coalg
  where
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = Mu (\alg -> fix (alg . Just))

toInt :: Nat -> Int
toInt = cata alg
  where
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Int -> Nat -> Bool
is n nat = cata alg nat n
 where
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
