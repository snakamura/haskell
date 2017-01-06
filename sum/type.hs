{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import Prelude hiding (sum)

data Z
data S n

type family Add n m
type instance Add Z m = m
type instance Add (S n) m = Add n (S m)

type family Sum n
type instance Sum Z = Z
type instance Sum (S n) = Add (S n) (Sum n)

class Value a where
    toValue :: Num b => a -> b
--instance Value Z where
--    toValue _ = 0
--instance Value n => Value (S n) where
--    toValue _ = 1 + toValue (undefined :: n)
instance Value (Proxy Z) where
    toValue _ = 0
instance Value (Proxy n) => Value (Proxy (S n)) where
    toValue _ = 1 + toValue (Proxy :: Proxy n)


sum :: forall a b. (Num a, Value (Proxy (Sum b))) => Proxy b -> a
sum _ = toValue (Proxy :: Proxy (Sum b))

sum5 :: Num a => a
sum5 = sum (Proxy :: Proxy (S (S (S (S (S Z))))))
