module NuNat where

import Data.Kind
import Prelude hiding (Maybe (..))

import Maybe
import Nu

type CoNat :: Type
type CoNat = Nu Maybe

projectCoNat :: CoNat -> Maybe CoNat
projectCoNat (Nu next state) =
  case next state of
    Just state' -> Just $ Nu next state'
    Nothing -> Nothing

zero', one' :: CoNat
zero' = Nu (\() -> Nothing) ()
one' = Nu f True
  where
    f False = Nothing
    f True = Just False

fromInt' :: Int -> CoNat
fromInt' = ana coalg
  where
    coalg 0 = Nothing
    coalg n' = Just $ n' - 1

inf' :: CoNat
inf' = ana (\_ -> Just ()) ()

toInt' :: CoNat -> Int
toInt' = cata alg
  where
    alg Nothing = 0
    alg (Just n) = n + 1

is' :: Int -> CoNat -> Bool
is' n coNat =
  case (project coNat, n) of
    (Just _, 0) -> False
    (Just coNat', n') -> is' (n' - 1) coNat'
    (Nothing, 0) -> True
    (Nothing, _) -> False
