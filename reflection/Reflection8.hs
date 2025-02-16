module Reflection8 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

type WrapMonoid :: k -> Type -> Type
newtype WrapMonoid s a = WrapMonoid a

type DictMonoid :: Type -> Type
data DictMonoid a = DictMonoid
  { _mempty :: a,
    _mappend :: a -> a -> a
  }

instance (Reifies s (DictMonoid a)) => Semigroup (WrapMonoid s a) where
  (WrapMonoid a1) <> (WrapMonoid a2) = WrapMonoid $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (WrapMonoid s a) where
  mempty = WrapMonoid $ _mempty (reflect (Proxy :: Proxy s))

v1 :: Int
v1 = reify
  DictMonoid
    { _mempty = 0,
      _mappend = (+)
    }
  $ \(_ :: Proxy s) ->
    let WrapMonoid n = mempty <> (WrapMonoid 10 :: WrapMonoid s Int) <> (WrapMonoid 20 :: WrapMonoid s Int)
     in n

v2 :: Int
v2 = reify
  DictMonoid
    { _mempty = 1,
      _mappend = (*)
    }
  $ \(_ :: Proxy s) ->
    let WrapMonoid n = mempty <> (WrapMonoid 10 :: WrapMonoid s Int) <> (WrapMonoid 20 :: WrapMonoid s Int)
     in n

withDictY :: forall a. DictMonoid a -> (forall k (s :: k). (Reifies s (DictMonoid a)) => WrapMonoid s a) -> a
withDictY dictY value = reify dictY $
  \(_ :: Proxy s) ->
    let WrapMonoid v :: WrapMonoid s a = value
     in v

v3 :: Int
v3 =
  withDictY
    DictMonoid
      { _mempty = 0,
        _mappend = (+)
      }
    $ mempty <> WrapMonoid 10 <> WrapMonoid 20

v4 :: Int
v4 =
  withDictY
    DictMonoid
      { _mempty = 1,
        _mappend = (*)
      }
    $ mempty <> WrapMonoid 10 <> WrapMonoid 20
