{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, PolyKinds, ScopedTypeVariables, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import Data.Singletons.Prelude (Sing, SingI, fromSing, sing)
import Data.Promotion.TH (promoteOnly)
import Data.Singletons.TH (singletons)
import Prelude hiding (sum)

singletons [d|
    data N = Z | S N
  |]

promoteOnly [d|
    add :: N -> N -> N
    add Z n = n
    add (S n) m = add n (S m)

    sum :: N -> N
    sum Z = Z
    sum n@(S n') = add n (sum n')
  |]

value :: Num a => N -> a
value Z = 0
value (S n) = 1 + value n

sum :: forall proxy (a :: N) b. (SingI (Sum a), Num b) => proxy (a :: N) -> b
sum p = value (fromSing (sing :: Sing (Sum a)))


sum5 :: Num a => a
sum5 = sum (sing :: SN (S (S (S (S (S Z))))))
sum5' = sum (Proxy :: Proxy (S (S (S (S (S Z))))))
