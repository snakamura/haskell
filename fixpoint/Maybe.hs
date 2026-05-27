{-# LANGUAGE Strict #-}

module Maybe where

import Data.Kind
import Prelude hiding (Maybe (..))

type Maybe :: Type -> Type
data Maybe a = Nothing | Just a deriving (Functor)

type Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (Mu fmuf) =
  let fa = cata alg <$> fmuf
   in alg fa

ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a =
  let fa = coalg a
   in Mu $ ana coalg <$> fa

type Nat :: Type
type Nat = Mu Maybe

zero, one, two :: Nat
zero = Mu Nothing
one = Mu (Just (Mu Nothing))
two = Mu (Just (Mu (Just (Mu Nothing))))

fromInt :: Int -> Nat
fromInt = ana coalg
  where
    coalg 0 = Nothing
    coalg n = Just $ n - 1

inf :: Nat
inf = Mu (Just inf)

toInt :: Nat -> Int
toInt = cata alg
  where
    alg Nothing = 0
    alg (Just n) = n + 1

is :: Int -> Nat -> Bool
is 0 (Mu Nothing) = True
is 0 _ = False
is n (Mu (Just mu)) = is (n - 1) mu
is _ _ = False

type Nu :: (Type -> Type) -> Type
data Nu f where
  Nu :: (s -> f s) -> s -> Nu f

cata' :: (Functor f) => (f a -> a) -> Nu f -> a
cata' alg nu =
  let fnuf = project nu
   in alg $ cata' alg <$> fnuf

ana' :: (Functor f) => (a -> f a) -> a -> Nu f
ana' = Nu

project :: (Functor f) => Nu f -> f (Nu f)
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
fromInt' = ana' coalg
  where
    coalg 0 = Nothing
    coalg n' = Just $ n' - 1

inf' :: CoNat
inf' = ana' (\_ -> Just ()) ()

toInt' :: CoNat -> Int
toInt' = cata' alg
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
