module Cont1 where

import Control.Monad

import Cont

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

boolToString :: Bool -> String
boolToString = show

composed :: Int -> String
composed = boolToString . isZero

isZeroCPS :: Int -> (Bool -> r) -> r
isZeroCPS 0 next = next True
isZeroCPS _ next = next False

boolToStringCPS :: Bool -> (String -> r) -> r
boolToStringCPS b next = next $ show b

composedCPS :: Int -> (String -> r) -> r
composedCPS n next = isZeroCPS n $ \b -> boolToStringCPS b $ \s -> next s

evalCont :: Cont r r -> r
evalCont (Cont ar2r) = ar2r id

isZeroCont :: Int -> Cont r Bool
isZeroCont 0 = Cont $ \b2r -> b2r True
isZeroCont _ = Cont $ \b2r -> b2r False

boolToStringCont :: Bool -> Cont r String
boolToStringCont b = Cont $ \s2r -> s2r (show b)

composedCont :: Int -> Cont r String
composedCont = isZeroCont >=> boolToStringCont
