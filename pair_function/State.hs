module State where

import Control.Comonad
import Data.Maybe

newtype State t a = State (t -> (t, a))

instance Functor (State t) where
  fmap :: (a -> b) -> State t a -> State t b
  fmap a2b (State t2ta) = State $ \t -> let (t', a) = t2ta t in (t', a2b a)

instance Applicative (State t) where
  pure :: a -> State t a
  pure a = State (, a)

  (<*>) :: State t (a -> b) -> State t a -> State t b
  State t2ta2b <*> State t2ta = State $ \t -> let (t', a2b) = t2ta2b t
                                                  (t'', a) = t2ta t'
                                               in (t'', a2b a)

instance Monad (State t) where
  (>>=) :: State t a -> (a -> State t b) -> State t b
  State t2ta >>= a2sb = State $ \t -> let (t', a) = t2ta t
                                          State t2tb = a2sb a
                                       in t2tb t'

withState :: (String, Int)
withState =
  let s1 :: Int -> State String String
      s1 n = State $ \t -> (t <> "!!!", show $ n + 10)
      s2 :: String -> State String Int
      s2 s = State $ \t -> (t <> s, length s)
      s3 :: Int -> State String Int
      s3 = pure
      state = "state"
      State t2ta = pure 100 >>= s1 >>= s2 >>= s3
   in t2ta state

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
