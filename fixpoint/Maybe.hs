{-# LANGUAGE Strict #-}

module Maybe where

import Data.Kind
import Prelude hiding (Maybe(..))

type Maybe :: Type -> Type
data Maybe a = Nothing | Just a deriving Functor

type Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

type Nat :: Type
type Nat = Mu Maybe

zero, one, two :: Nat
zero = Mu Nothing
one = Mu (Just (Mu Nothing))
two = Mu (Just (Mu (Just (Mu Nothing))))

fromInt :: Int -> Nat
fromInt 0 = Mu Nothing
fromInt n = Mu $ Just $ fromInt $ n - 1

inf :: Nat
inf = Mu (Just inf)

is :: Int -> Nat -> Bool
is 0 (Mu Nothing) = True
is 0 _ = False
is n (Mu (Just mu)) = is (n - 1) mu
is _ _ = False

type Nu :: (Type -> Type) -> Type
data Nu f where
  Nu :: (s -> f s) -> s -> Nu f

project :: Functor f => Nu f -> f (Nu f)
project (Nu next state) = Nu next <$> next state

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
fromInt' n = Nu f n
  where
    f 0 = Nothing
    f n' = Just $ n' - 1

inf' :: CoNat
inf' = Nu (\_ -> Just ()) ()

is' :: Int -> CoNat -> Bool
is' n coNat =
  case (project coNat, n) of
    (Just _, 0) -> False
    (Just coNat', n') -> is' (n' - 1) coNat'
    (Nothing, 0) -> True
    (Nothing, _) -> False
