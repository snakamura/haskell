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
  (Wrap a1) <> (Wrap a2) = Wrap $ _mappend (reflect (Proxy :: Proxy s)) a1 a2

instance (Reifies s (DictMonoid a)) => Monoid (Wrap s a) where
  mempty = Wrap $ _mempty (reflect (Proxy :: Proxy s))

v1 :: Int
v1 = reify
  DictMonoid
    { _mempty = 0,
      _mappend = (+)
    }
  $ \(_ :: Proxy s) ->
    let Wrap n = mempty <> (Wrap 10 :: Wrap s Int) <> (Wrap 20 :: Wrap s Int)
     in n

v2 :: Int
v2 = reify
  DictMonoid
    { _mempty = 1,
      _mappend = (*)
    }
  $ \(_ :: Proxy s) ->
    let Wrap n = mempty <> (Wrap 10 :: Wrap s Int) <> (Wrap 20 :: Wrap s Int)
     in n

withDictMonoid :: forall a. DictMonoid a -> (forall k (s :: k). (Reifies s (DictMonoid a)) => Wrap s a) -> a
withDictMonoid dictY value = reify dictY $
  \(_ :: Proxy s) ->
    let Wrap v :: Wrap s a = value
     in v

v3 :: Int
v3 =
  withDictMonoid
    DictMonoid
      { _mempty = 0,
        _mappend = (+)
      }
    $ mempty <> Wrap 10 <> Wrap 20

v4 :: Int
v4 =
  withDictMonoid
    DictMonoid
      { _mempty = 1,
        _mappend = (*)
      }
    $ mempty <> Wrap 10 <> Wrap 20
