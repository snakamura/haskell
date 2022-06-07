{-# LANGUAGE DerivingVia,
             InstanceSigs,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TypeFamilies
#-}

import Data.Kind (Constraint, Type)
import Numeric.Natural (Natural)

type Representable :: (Type -> Type)-> Constraint
class Functor f => Representable f where
    type Rep f :: Type
    tabulate :: (Rep f -> a) -> f a
    index :: f a -> Rep f -> a

-- Identity
newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Representable Identity where
    type Rep Identity = ()

    tabulate :: (() -> a) -> Identity a
    tabulate f = Identity (f ())

    index :: Identity a -> () -> a
    index (Identity x) () = x

indexedIdentity :: () -> Int
indexedIdentity = index (Identity 5)

tabulatedIdentity :: Identity Int
tabulatedIdentity = tabulate indexedIdentity

-- Stream
data Stream a = Stream a (Stream a)

takeStream :: Int -> Stream a -> [a]
takeStream 0 _ = []
takeStream n (Stream x s) = x : takeStream (n - 1) s

instance Functor Stream where
    fmap f (Stream x s) = Stream (f x) (fmap f s)

instance Representable Stream where
    type Rep Stream = Natural

    tabulate :: (Natural -> a) -> Stream a
    tabulate f = go 0
        where
            go n = Stream (f n) (go (n + 1))

    index :: Stream a -> Natural -> a
    index (Stream x _) 0 = x
    index (Stream _ s) n = index s (n - 1)

stream :: Stream Int
stream = Stream 0 (Stream 1 (Stream 2 (Stream 3 (Stream 4 stream))))

indexedStream :: Natural -> Int
indexedStream = index stream

tabulatedStream :: Stream Int
tabulatedStream = tabulate indexedStream

-- Pair
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Representable Pair where
    type Rep Pair = Bool

    tabulate :: (Bool -> a) -> Pair a
    tabulate f = Pair (f True) (f False)

    index :: Pair a -> Bool -> a
    index (Pair x _) True = x
    index (Pair _ y) False = y

indexedPair :: Bool -> String
indexedPair = index (Pair "x" "y")

tabulatedPair :: Pair String
tabulatedPair = tabulate indexedPair


newtype Wrap f a = Wrap { unWrap :: f a } deriving Show

instance Functor f => Functor (Wrap f) where
    fmap :: (a -> b) -> (Wrap f a -> Wrap f b)
    fmap f (Wrap x) = Wrap (fmap f x)

instance Representable f => Representable (Wrap f) where
    type Rep (Wrap f) = Rep f

    tabulate :: (Rep (Wrap f) -> a) -> Wrap f a
    tabulate f = Wrap (tabulate f)

    index :: Wrap f a -> Rep (Wrap f) -> a
    index (Wrap x) = index x

instance Representable f => Applicative (Wrap f) where
    pure :: a -> Wrap f a
    pure = tabulate . const

    (<*>) :: Wrap f (a -> b) -> Wrap f a -> Wrap f b
    f <*> g = tabulate (index f <*> index g)

instance Representable f => Monad (Wrap f) where
    (>>=) :: Wrap f a -> (a -> Wrap f b) -> Wrap f b
    m >>= f = tabulate $ \x -> index (f (index m x)) x


deriving via Wrap Identity instance Applicative Identity
deriving via Wrap Identity instance Monad Identity

deriving via Wrap Stream instance Applicative Stream
deriving via Wrap Stream instance Monad Stream

deriving via Wrap Pair instance Applicative Pair
deriving via Wrap Pair instance Monad Pair
