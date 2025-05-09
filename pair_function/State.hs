module State where

import Control.Comonad
import Data.Maybe

newtype State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap a2b (State s2sa) = State $ \s -> let (s', a) = s2sa s in (s', a2b a)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (s, a))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State s2sa2b <*> State s2sa = State $ \s -> let (s', a2b) = s2sa2b s
                                                    (s'', a) = s2sa s'
                                                 in (s'', a2b a)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    State s2sa >>= a2sb = State $ \s -> let (s', a) = s2sa s
                                            State s2sb = a2sb a
                                         in s2sb s'

withState :: (String, Int)
withState = let state = "state"
                s1 :: Int -> State String String
                s1 n = State $ \s -> (s <> "!!!", show $ n + 10)
                s2 :: String -> State String Int
                s2 n = State $ \s -> (s <> n, length n)
                s3 :: Int -> State String Int
                s3 = pure
                State s2sa = pure 100 >>= s1 >>= s2 >>= s3
              in s2sa state

newtype Store s a = Store (s, s -> a)

instance Functor (Store s) where
    fmap :: (a -> b) -> Store s a -> Store s b
    fmap a2b (Store (s, s2a)) = Store (s, a2b . s2a)

instance Comonad (Store s) where
    extract :: Store s a -> a
    extract (Store (s, s2a)) = s2a s

    extend :: (Store s a -> b) -> Store s a -> Store s b
    extend sa2b (Store (s, s2a)) = Store (s, \s' -> sa2b (Store (s', s2a)))

    --duplicate :: Store s a -> Store s (Store s a)
    --duplicate (Store (s, s2a)) = Store (s, \s' -> Store (s', s2a))

withStore :: ([Int], Int)
withStore = let store :: [(Int, String)] = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
                s1 :: Store Int String -> (String, String)
                s1 (Store (s, s2a)) = (s2a (s - 1), s2a (s + 1))
                s2 :: Store Int (String, String) -> Int
                s2 (Store (s, s2a)) = length $ uncurry (<>) $ s2a s
                s3 :: Store Int Int -> Int
                s3 = extract
                Store (s', s2a') = Store (3, \s -> fromMaybe "" $ lookup s store) =>> s1 =>> s2 =>> s3
            in (map s2a' [1..5], s2a' s')
