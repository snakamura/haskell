module NuNat where

import Data.Kind
import Prelude hiding (Maybe (..))

import Maybe
import Nu

type Nat :: Type
type Nat = Nu Maybe

projectNat :: Nat -> Maybe Nat
projectNat (Nu next state) =
  case next state of
    Just state' -> Just $ Nu next state'
    Nothing -> Nothing

zero, one :: Nat
zero = Nu (\() -> Nothing) ()
one = Nu f True
  where
    f False = Nothing
    f True = Just False

fromInt :: Int -> Nat
fromInt = ana coalg
  where
    coalg :: Int -> Maybe Int
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = ana (\_ -> Just ()) ()

toInt :: Nat -> Int
toInt = cata alg
  where
    alg :: Maybe Int -> Int
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Int -> Nat -> Bool
is n nat = case (project nat, n) of
    (Nothing, 0) -> True
    (Nothing, _) -> False
    (Just nat', n') -> n' > 0 && is' (n' - 1) nat'

is' :: Int -> Nat -> Bool
is' n nat = cata alg nat n
 where
    alg :: Maybe (Int -> Bool) -> (Int -> Bool)
    alg Nothing = (== 0)
    alg (Just f) = \n' -> n' > 0 && f (n' - 1)
