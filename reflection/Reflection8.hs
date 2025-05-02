module Reflection8 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap a

type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance (Reifies s (DictMonoid a)) => Semigroup (Wrap s a) where
  Wrap a1 <> Wrap a2 = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))

v :: (Reifies s (DictMonoid Int)) => Wrap s Int
v = mempty <> Wrap 10 <> Wrap 20

v1 :: Int
v1 = reify (DictMonoid 0 (+)) $ \(_ :: Proxy s) -> let Wrap n = v @s in n

v2 :: Int
v2 = reify (DictMonoid 1 (*)) $ \(_ :: Proxy s) -> let Wrap n = v @s in n

withDictMonoid :: forall a. DictMonoid a -> (forall k (s :: k). (Reifies s (DictMonoid a)) => Wrap s a) -> a
withDictMonoid dictY value = reify dictY $
  \(_ :: Proxy s) -> let Wrap value' :: Wrap s a = value in value'

v3 :: Int
v3 = withDictMonoid (DictMonoid 0 (+)) v

v4 :: Int
v4 = withDictMonoid (DictMonoid 1 (*)) v
