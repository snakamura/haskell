module Store where

import Control.Comonad
import Data.Maybe

newtype Store t a = Store (t, t -> a)

instance Functor (Store t) where
  fmap :: (a -> b) -> Store t a -> Store t b
  fmap a2b (Store (t, t2a)) = Store (t, a2b . t2a)

instance Comonad (Store k) where
  extract :: Store k a -> a
  extract (Store (k, k2a)) = k2a k

  extend :: (Store k a -> b) -> Store k a -> Store k b
  extend sa2b (Store (k, k2a)) = Store (k, \k' -> sa2b (Store (k', k2a)))

withStore :: ([Int], Int)
withStore =
  let s1 :: Store Int String -> (String, String)
      s1 (Store (n, n2s)) = (n2s (n - 1), n2s (n + 1))
      s2 :: Store Int (String, String) -> Int
      s2 (Store (n, n2ss)) = length $ uncurry (<>) $ n2ss n
      s3 :: Store Int Int -> Int
      s3 = extract
      store :: [(Int, String)] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
      Store (k', k2a') = Store (3, \k -> fromMaybe "" $ lookup k store) =>> s1 =>> s2 =>> s3
   in (map k2a' [1..5], k2a' k')
