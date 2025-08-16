module Store where

import Control.Comonad
import Data.Maybe

newtype Store t a = Store (t, t -> a)

instance Functor (Store t) where
  fmap :: (a -> b) -> Store t a -> Store t b
  fmap a2b (Store (t, t2a)) = Store (t, a2b . t2a)

instance Comonad (Store t) where
  extract :: Store t a -> a
  extract (Store (t, t2a)) = t2a t

  extend :: (Store t a -> b) -> Store t a -> Store t b
  extend sa2b (Store (t, t2a)) = Store (t, \t' -> sa2b (Store (t', t2a)))

  duplicate :: Store t a -> Store t (Store t a)
  duplicate (Store (t, t2a)) = Store (t, \t' -> Store (t', t2a))

pos :: Store t a -> t
pos (Store (t, _)) = t

peek :: t -> Store t a -> a
peek t (Store (_, t2a)) = t2a t

peeks :: (t -> t) -> Store t a -> a
peeks f (Store (t, t2a)) = t2a (f t)

seek :: t -> Store t a -> Store t a
seek t (Store (_, t2a)) = Store (t, t2a)

seeks :: (t -> t) -> Store t a -> Store t a
seeks f (Store (t, t2a)) = Store (f t, t2a)

withStore :: ([Int], Int)
withStore =
  let s1 :: Store Int String -> (String, String)
      s1 (Store (n, n2s)) = (n2s (n - 1), n2s (n + 1))
      s2 :: Store Int (String, String) -> Int
      s2 (Store (n, n2ss)) = length $ uncurry (<>) $ n2ss n
      s3 :: Store Int Int -> Int
      s3 (Store (n, n2n)) = n2n (n * 2)
      store :: [(Int, String)] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      Store (t', t2a') = Store (3, \k -> fromMaybe "" $ lookup k store) =>> s1 =>> s2 =>> s3
   in (map t2a' [1 .. 5], t2a' t')

withStore' :: ([Int], Int)
withStore' =
  let s1 :: Store Int String -> (String, String)
      s1 s = (peeks (subtract 1) s, peeks (+ 1) s)
      s2 :: Store Int (String, String) -> Int
      s2 s = length $ uncurry (<>) $ extract s
      s3 :: Store Int Int -> Int
      s3 s = extract $ seeks (* 2) s
      store :: [(Int, String)] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      Store (t', t2a') = Store (3, \t -> fromMaybe "" $ lookup t store) =>> s1 =>> s2 =>> s3
   in (map t2a' [1 .. 5], t2a' t')

withStore'' :: Int
withStore'' =
  let s1 :: Store Int String -> (String, String)
      s1 (Store (n, n2s)) = (n2s (n - 1), n2s (n + 1))
      s2 :: Store Int (String, String) -> Int
      s2 (Store (n, n2ss)) = length $ uncurry (<>) $ n2ss n
      s3 :: Store Int Int -> Int
      s3 (Store (n, n2n)) = n2n (n * 2)
      store :: [(Int, String)] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      a' = s3 $ fmap s2 $ fmap (fmap s1) $ duplicate $ duplicate $ Store (3, \k -> fromMaybe "" $ lookup k store)
   in a'
