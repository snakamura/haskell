{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes #-}

import Data.Distributive
import Data.Functor.Rep
import Numeric.Natural

fibRec :: Natural -> Integer
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) + fibRec (n - 2)


data Stream a = Stream a (Stream a) deriving (Show, Functor)

atStream :: Stream a -> Natural -> a
atStream (Stream a _) 0 = a
atStream (Stream _ as) n = atStream as (n - 1)


toStream :: (Natural -> a) -> Stream a
toStream f = fmap f naturals
  where
    naturals = Stream 0 (fmap (+1) naturals)

fromStream :: Stream a -> (Natural -> a)
fromStream = atStream


fibs :: Stream Integer
fibs = toStream fibRec

fibWithStream :: Natural -> Integer
fibWithStream = fromStream fibs


fibF :: (Natural -> Integer) -> (Natural -> Integer)
fibF _ 0 = 0
fibF _ 1 = 1
fibF f n = f (n - 1) + f (n - 2)

fib1, fib2, fib3 :: Natural -> Integer
fib1 = fibF undefined -- Works up to 1
fib2 = fibF (fibF undefined) -- Works up to 2
fib3 = fibF (fibF (fibF undefined)) -- Works up to 3
-- fibN = fibF fibN_1

fixFun :: ((Natural -> Integer) -> (Natural -> Integer)) -> (Natural -> Integer)
fixFun f = f (fixFun f) -- f (f (f (f (f (f ...)))))

fibFix :: Natural -> Integer
fibFix = fixFun fibF

{-
tailStream :: Stream a -> Stream a
tailStream (Stream a as) = as

zipStreamWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreamWith f (Stream a as) (Stream b bs)= Stream (f a b) (zipStreamWith f as bs)

fibsStream :: Num a => Stream a
fibsStream = Stream 0 (Stream 1 (Stream 1 (zipStreamWith (+) t1 t2)))
  where
    t1 = tailStream fibsStream
    t2 = tailStream t1

fibStream :: Num a => Natural -> a
fibStream n = atStream n fibsStream
-}


fib1', fib2', fib3' :: Natural -> Integer
fib1' = fibF undefined -- Works up to 1
fib2' = fibF (fromStream (toStream (fibF undefined))) -- Works up to 2
fib3' = fibF (fromStream (toStream (fibF (fromStream (toStream (fibF undefined)))))) -- Works up to 3
-- fibN' = fibF (fromStream . toStream . fibN_1)

fib :: Natural -> Integer
fib = fixFun (fromStream . toStream . fibF)


instance Representable Stream where
    type Rep Stream = Natural
    tabulate = toStream
    index = fromStream

instance Distributive Stream where
    distribute = distributeRep


data Id a = Id a deriving Functor

instance Representable Id where
    type Rep Id = ()
    tabulate f = Id (f ())
    index (Id a) () = a

instance Distributive Id where
    distribute = distributeRep


data Pair a = Pair a a deriving Functor

instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) True = x
    index (Pair x y) False = y

instance Distributive Pair where
    distribute = distributeRep


fix :: (a -> a) -> a
fix f = f (fix f)

memoize :: forall f a. Representable f => ((Rep f -> a) -> (Rep f -> a)) -> (Rep f -> a)
memoize g = fix (index . tabulate @f . g)

fib' :: Natural -> Integer
fib' = memoize @Stream fibF
