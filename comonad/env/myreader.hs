{-# LANGUAGE InstanceSigs #-}

import Control.Monad ((>=>))

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
    fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader e) where
    pure x = Reader $ \e -> x
    (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
    Reader f <*> Reader x = Reader $ \e -> f e (x e)

instance Monad (Reader e) where
    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    Reader x >>= f = Reader $ \e -> let Reader y = f (x e) in y e

ask :: Reader e e
ask = Reader id


add :: Float -> Reader Int Float
add n = ask >>= \e -> return $ n + fromIntegral e

mul :: Float -> Reader Int Float
mul n = ask >>= \e -> return $ n * fromIntegral e

result, result' :: Float
result = runReader (add 2 >>= mul) 3
result' = runReader ((add >=> mul) 2) 3
