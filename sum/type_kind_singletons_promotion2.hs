{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, PolyKinds, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import Data.Singletons.Prelude (FalseSym0, PEq, Sing(STrue, SFalse), SingI, TrueSym0, fromSing, sing)
import Data.Promotion.TH (promote)
import Data.Singletons.TH (singletons)
import Prelude hiding (sum)

singletons [d|
    data N = Z | S N deriving Eq
  |]

promote [d|
    add :: N -> N -> N
    add Z n = n
    add (S n) m = add n (S m)

    sum :: N -> N
    sum = fold add Z

    fold :: (N -> a -> a) -> a -> N -> a
    fold f i n = case n of
                   Z -> i
                   (S m) -> fold f o m
      where
        o = f n i
  |]

value :: Num a => N -> a
value = fold (const (+ 1)) 0

sum' :: forall proxy (a :: N) b. (SingI (Sum a), Num b) => proxy (a :: N) -> b
sum' p = value (fromSing (sing :: Sing (Sum a)))


sum5 :: Num a => a
sum5 = sum' (sing :: SN (S (S (S (S (S Z))))))
sum5' = sum' (Proxy :: Proxy (S (S (S (S (S Z))))))
