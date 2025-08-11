module Env where

import Control.Comonad

newtype Env v a = Env (v, a)

instance Functor (Env v) where
  fmap :: (a -> b) -> Env v a -> Env v b
  fmap a2b (Env (v, a)) = Env (v, a2b a)

instance Comonad (Env v) where
  extract :: Env v a -> a
  extract (Env (_, a)) = a

  extend :: (Env v a -> b) -> Env v a -> Env v b
  extend ea2b ea@(Env (v, _)) = Env (v, ea2b ea)

withEnv :: Int
withEnv =
  let e1 :: Env Int Int -> String
      e1 (Env (v, n)) = show $ v + n + 1
      e2 :: Env Int String -> Int
      e2 (Env (v, s)) = v * (length s + 2)
      e3 :: Env Int Int -> Int
      e3 = extract
      env = 10
      Env (_, a) = Env (env, 100) =>> e1 =>> e2 =>> e3
   in a
