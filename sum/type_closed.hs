{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import Prelude hiding (sum)

data Z
data S n

type family Add n m where
    Add Z m = m
    Add (S n) m = Add n (S m)

type family Sum n where
    Sum Z = Z
    Sum (S n) = Add (S n) (Sum n)

class Value a where
    toValue :: Num b => a -> b
instance Value (Proxy Z) where
    toValue _ = 0
instance Value (Proxy n) => Value (Proxy (S n)) where
    toValue _ = 1 + toValue (Proxy :: Proxy n)

sum :: forall a b. (Num a, Value (Proxy (Sum b))) => Proxy b -> a
sum _ = toValue (Proxy :: Proxy (Sum b))


sum5 :: Num a => a
sum5 = sum (Proxy :: Proxy (S (S (S (S (S Z))))))
