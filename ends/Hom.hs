module Hom where

import Data.List
import Data.Profunctor

newtype Hom f g a b = Hom (f a -> g b)

type List2Maybe a b = Hom List Maybe a b

hom1 :: List2Maybe Int Int
hom1 = Hom $ \cases
  xs | 0 `elem` xs -> Nothing
  [] -> Nothing
  (x : _) -> Just x

hom2 :: List2Maybe Int Int
hom2 = Hom $ \cases
  [] -> Nothing
  (x : _) -> Just x

hom3 :: List2Maybe Int Int
hom3 = Hom go
  where
    go [] = Nothing
    go [x] = Just x
    go (_ : xs) = go xs

instance (Functor f, Functor g) => Profunctor (Hom f g) where
  dimap :: (a' -> a) -> (b -> b') -> Hom f g a b -> Hom f g a' b'
  dimap a'2a b2b' (Hom h) = Hom $ fmap b2b' . h . fmap a'2a

tau1Int :: () -> List2Maybe Int Int
tau1Int () = hom1

tau2Int :: () -> List2Maybe Int Int
tau2Int () = hom2

tau3Int :: Bool -> List2Maybe Int Int
tau3Int True = hom2
tau3Int False = hom3

-- fmap h . hom === hom . fmap h
-- lmap h hom == rmap h hom
-- lmap h (tau x) === rmap h (tau x)

l1, r1 :: Maybe Int
l1 =
  -- Just 100
  let Hom hom = lmap (+ 100) (tau1Int ())
   in hom [0, 1, 2]
r1 =
  -- Nothing
  let Hom hom = rmap (+ 100) (tau1Int ())
   in hom [0, 1, 2]

l2, r2 :: Maybe Int
l2 =
  -- Just 100
  let Hom hom = lmap (+ 100) (tau2Int ())
   in hom [0, 1, 2]
r2 =
  -- Just 100
  let Hom hom = rmap (+ 100) (tau2Int ())
   in hom [0, 1, 2]

l3_1, l3_2, r3_1, r3_2 :: Maybe Int
l3_1 =
  -- Just 100
  let Hom hom = lmap (+ 100) (tau3Int True)
   in hom [0, 1, 2]
l3_2 =
  -- Just 102
  let Hom hom = lmap (+ 100) (tau3Int False)
   in hom [0, 1, 2]
r3_1 =
  -- Just 100
  let Hom hom = rmap (+ 100) (tau3Int True)
   in hom [0, 1, 2]
r3_2 =
  -- Just 102
  let Hom hom = rmap (+ 100) (tau3Int False)
   in hom [0, 1, 2]

nt2 :: List2Maybe a a
nt2 = Hom $ \cases
  [] -> Nothing
  (x : _) -> Just x

nt3 :: List2Maybe a a
nt3 = Hom go
  where
    go [] = Nothing
    go [x] = Just x
    go (_ : xs) = go xs

tau2 :: () -> List2Maybe a a
tau2 () = nt2

tau3 :: Bool -> List2Maybe a a
tau3 True = nt2
tau3 False = nt3

l2', r2' :: Maybe Bool
l2' =
  -- Just True
  let Hom hom = lmap (== 0) (tau2 ())
   in hom [0, 1, 2]
r2' =
  -- Just True
  let Hom hom = rmap (== 0) (tau2 ())
   in hom [0, 1, 2]

l3_1', l3_2', r3_1', r3_2' :: Maybe Bool
l3_1' =
  -- Just True
  let Hom hom = lmap (== 0) (tau3 True)
   in hom [0, 1, 2]
l3_2' =
  -- Just False
  let Hom hom = lmap (== 0) (tau3 False)
   in hom [0, 1, 2]
r3_1' =
  -- Just True
  let Hom hom = rmap (== 0) (tau3 True)
   in hom [0, 1, 2]
r3_2' =
  -- Just False
  let Hom hom = rmap (== 0) (tau3 False)
   in hom [0, 1, 2]
