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

  duplicate :: Env v a -> Env v (Env v a)
  duplicate (Env (v, a)) = Env (v, Env (v, a))

ask :: Env v a -> v
ask (Env (v, _)) = v

local :: (v -> v) -> Env v a -> Env v a
local v2v (Env (v, a)) = Env (v2v v, a)

withEnv :: Int
withEnv =
  let e1 :: Env Int Int -> String
      e1 (Env (v, n)) = show $ v + n + 1
      e2 :: Env Int String -> Int
      e2 (Env (v, s)) = v * (length s + 2)
      e3 :: Env Int Int -> Int
      e3 (Env (v, n)) = let e3' (Env (v', n')) = v' + n' in e3' (Env (v * 2, n))
      env = 10
      Env (_, a) = Env (env, 100) =>> e1 =>> e2 =>> e3
   in a

withEnv' :: Int
withEnv' =
  let e1 :: Env Int Int -> String
      e1 e = show $ ask e + extract e + 1
      e2 :: Env Int String -> Int
      e2 e = ask e * (length (extract e) + 2)
      e3 :: Env Int Int -> Int
      e3 e = let e3' e' = ask e' + extract e' in e3' $ local (* 2) e
      env = 10
      Env (_, a) = Env (env, 100) =>> e1 =>> e2 =>> e3
   in a
