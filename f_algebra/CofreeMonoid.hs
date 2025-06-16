{-# LANGUAGE UndecidableInstances #-}

module CofreeMonoid where

data CofreeMonoid a r = Append r r
                      | Empty
                      | Lift a
  deriving Show

instance Functor (CofreeMonoid a) where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
    fmap _ (Lift n) = Lift n

sumIntCoalg :: Int -> CofreeMonoid Int Int
sumIntCoalg 0 = Empty
sumIntCoalg n = Append n 0

v1, v2 :: CofreeMonoid Int Int
v1 = sumIntCoalg 0
v2 = sumIntCoalg 3

sumMonoidIntCoalg :: CofreeMonoid Int Int -> CofreeMonoid Int (CofreeMonoid Int Int)
sumMonoidIntCoalg Empty = Empty
sumMonoidIntCoalg (Append m n) = Append (sumIntCoalg m) (sumIntCoalg n)
sumMonoidIntCoalg (Lift n) = Lift n

v3 :: CofreeMonoid Int (CofreeMonoid Int Int)
v3 = sumMonoidIntCoalg $ sumIntCoalg 3

sumMonoidMonoidIntCoalg :: CofreeMonoid Int (CofreeMonoid Int Int) -> CofreeMonoid Int (CofreeMonoid Int (CofreeMonoid Int Int))
sumMonoidMonoidIntCoalg Empty = Empty
sumMonoidMonoidIntCoalg (Append m n) = Append (sumMonoidIntCoalg m) (sumMonoidIntCoalg n)
sumMonoidMonoidIntCoalg (Lift n) = Lift n

v4 :: CofreeMonoid Int (CofreeMonoid Int (CofreeMonoid Int Int))
v4 = sumMonoidMonoidIntCoalg $ sumMonoidIntCoalg $ sumIntCoalg 3

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)

out :: Fix f -> f (Fix f)
out (In f) = f

terminalCoalg :: Fix (CofreeMonoid a) -> CofreeMonoid a (Fix (CofreeMonoid a))
terminalCoalg = out

homSumIntToTerminal :: Int -> Fix (CofreeMonoid Int)
homSumIntToTerminal = In . fmap homSumIntToTerminal . sumIntCoalg

m0, m1 :: Fix (CofreeMonoid Int)
m0 = homSumIntToTerminal 0
m1 = homSumIntToTerminal 3

homIntToTerminal :: (Int -> CofreeMonoid Int Int) -> Int -> Fix (CofreeMonoid Int)
homIntToTerminal coalg = In . fmap (homIntToTerminal coalg) . coalg

hom :: (a -> CofreeMonoid a a) -> a -> Fix (CofreeMonoid a)
hom coalg = In . fmap (hom coalg) . coalg

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = In . fmap (ana coalg) . coalg
