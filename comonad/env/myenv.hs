{-# LANGUAGE InstanceSigs #-}

import Control.Comonad
    ( Comonad
    , (=>>)
    , (=>=)
    , extract
    , extend
    )

newtype Env e a = Env (e, a)

instance Functor (Env e) where
    fmap f (Env (e, x)) = Env (e, f x)

instance Comonad (Env e) where
    extract :: Env e a -> a
    extract (Env (_, a)) = a
    extend :: (Env e a -> b) -> Env e a -> Env e b
    extend f env@(Env (e, x)) = Env (e, f env)

ask :: Env e a -> e
ask (Env (e, _)) = e


add :: Env Int Float -> Float
add env = fromIntegral (ask env) + extract env

mul :: Env Int Float -> Float
mul env = fromIntegral (ask env) * extract env

result, result' :: Float
result = extract $ Env (3, 2) =>> add =>> mul
result' = (add =>= mul) $ Env (3, 2)
