{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, PolyKinds, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import Data.Singletons.Prelude (Sing, SingI, fromSing, sing)
import Data.Singletons.TH (singletons)
import Prelude hiding (sum)

singletons [d|
    data N = Z | S N
  |]

type family Add n m where
    Add Z m = m
    Add (S n) m = Add n (S m)

type family Sum n where
    Sum Z = Z
    Sum (S n) = Add (S n) (Sum n)

toNum :: Num a => N -> a
toNum Z = 0
toNum (S n) = 1 + toNum n

sum :: forall proxy (a :: N) b. (SingI (Sum a), Num b) => proxy (a :: N) -> b
sum p = toNum (fromSing (sing :: Sing (Sum a)))

sum5 :: Num a => a
sum5 = sum (sing :: Sing (S (S (S (S (S Z))))))
sum5' = sum (Proxy :: Proxy (S (S (S (S (S Z))))))


{-
fromNum :: (Eq a, Num a) => a -> N
fromNum 0 = Z
fromNum n = S (fromNum (n - 1))

x = let y = toSing Z :: SomeSing N
    in case y of
         SomeSing SZ -> undefined
         SomeSing (SS SZ) -> undefined

f :: Sing a -> Proxy a
f _ = Proxy

y :: N -> Proxy a
y n = withSomeSing n f
-}
