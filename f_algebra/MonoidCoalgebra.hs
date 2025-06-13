{-# LANGUAGE UndecidableInstances #-}

module MonoidCoalgebra where

import Prelude hiding (Monoid)

data Monoid a = Append a a
              | Empty
  deriving Show

instance Functor Monoid where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty

sumIntCoalg :: Int -> Monoid Int
sumIntCoalg 0 = Empty
sumIntCoalg n = let m = n `div` 2 in Append m (n - m)

v1, v2 :: Monoid Int
v1 = sumIntCoalg 0
v2 = sumIntCoalg 3

sumMonoidIntCoalg :: Monoid Int -> Monoid (Monoid Int)
sumMonoidIntCoalg Empty = Empty
sumMonoidIntCoalg (Append m n) = Append (sumIntCoalg m) (sumIntCoalg n)

v3 :: Monoid (Monoid Int)
v3 = sumMonoidIntCoalg $ sumIntCoalg 3

sumMonoidMonoidIntCoalg :: Monoid (Monoid Int) -> Monoid (Monoid (Monoid Int))
sumMonoidMonoidIntCoalg Empty = Empty
sumMonoidMonoidIntCoalg (Append m n) = Append (sumMonoidIntCoalg m) (sumMonoidIntCoalg n)

v4 :: Monoid (Monoid (Monoid Int))
v4 = sumMonoidMonoidIntCoalg $ sumMonoidIntCoalg $ sumIntCoalg 3

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)

out :: Fix f -> f (Fix f)
out (In f) = f

terminalCoalg :: Fix Monoid -> Monoid (Fix Monoid)
terminalCoalg = out

homSumIntToTerminal :: Int -> Fix Monoid
homSumIntToTerminal = In . fmap homSumIntToTerminal . sumIntCoalg

m0, m1 :: Fix Monoid
m0 = homSumIntToTerminal 0
m1 = homSumIntToTerminal 3

homIntToTerminal :: (Int -> Monoid Int) -> Int -> Fix Monoid
homIntToTerminal coalg = In . fmap (homIntToTerminal coalg) . coalg

hom :: (a -> Monoid a) -> a -> Fix Monoid
hom coalg = In . fmap (hom coalg) . coalg

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg
