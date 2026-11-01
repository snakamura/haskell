module Cont1 where

import Control.Monad

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

newtype Cont r a = Cont ((a -> r) -> r)

evalCont :: Cont r r -> r
evalCont (Cont ar2r) = ar2r id

instance Functor (Cont r) where
  fmap :: (a -> b) -> (Cont r a -> Cont r b)
  fmap a2b (Cont ar2r) = Cont $ \b2r -> ar2r (\a -> b2r (a2b a))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont $ \a2r -> a2r a

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) (Cont abr2r) (Cont ar2r) = Cont $ \b2r -> abr2r $ \a2b -> ar2r $ \a -> b2r (a2b a)

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) (Cont ar2r) a2crb = Cont $ \b2r -> ar2r $ \a -> let (Cont br2r) = a2crb a in br2r b2r

isZeroCont :: Int -> Cont r Bool
isZeroCont 0 = Cont $ \b2r -> b2r True
isZeroCont _ = Cont $ \b2r -> b2r False

boolToStringCont :: Bool -> Cont r String
boolToStringCont b = Cont $ \s2r -> s2r (show b)

composedCont :: Int -> Cont r String
composedCont = isZeroCont >=> boolToStringCont
