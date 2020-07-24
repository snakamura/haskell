{-# LANGUAGE InstanceSigs #-}

import Control.Monad ((>=>))
import Data.Functor (($>))

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (x, w)) = Writer (f x, w)

instance Monoid w => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    Writer (f, fw) <*> Writer (x, xw) = Writer (f x, fw <> xw)

instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    Writer (x, xw) >>= f = let Writer (y, yw) = f x in Writer (y, xw <> yw)

tell :: w -> Writer w ()
tell x = Writer ((), x)


add :: Int -> Writer String Int
add v = tell ("Adding 10 to " ++ show v ++ "\n") $> v + 10

mul :: Int -> Writer String Int
mul v = tell ("Multiplying " ++ show v ++ " by 2\n") $> v * 2

result, result' :: (Int, String)
result = runWriter (add 5 >>= mul)
result' = runWriter ((add >=> mul) 5)
