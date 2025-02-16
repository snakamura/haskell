{-# LANGUAGE UndecidableInstances #-}

module Reflection6 where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Exts (Any, withDict)
import Unsafe.Coerce (unsafeCoerce)

type Bind :: k -> Type -> Constraint
class Bind s a | s -> a where
  get :: Proxy s -> a

bind :: forall a r. a -> (forall k (s :: k). (Bind s a) => Proxy s -> r) -> r
bind a f =
  withDict
    @(Bind (Any @Any) a)
    (const a)
    (f @Any @Any)
    Proxy

type Magic :: Type -> Type -> Type
newtype Magic a r = Magic (forall k (s :: k). (Bind s a) => Proxy s -> r)

unsafeBind :: a -> (forall k (s :: k). (Bind s a) => Proxy s -> r) -> r
unsafeBind a f = unsafeCoerce (Magic f) (const a) (Proxy :: Proxy s)

class Y a where
  y1 :: a -> String
  y2 :: a -> a -> Int

instance Y String where
  y1 = id
  y2 _ _ = 0

instance Y Int where
  y1 = show
  y2 = (+)

type WrapY :: k -> Type -> Type
newtype WrapY s a = WrapY a

type DictY :: Type -> Type
data DictY a = DictY
  { _y1 :: a -> String,
    _y2 :: a -> a -> Int
  }

instance (Bind s (DictY a)) => Y (WrapY s a) where
  y1 (WrapY a) = _y1 (get (Proxy :: Proxy s)) a
  y2 (WrapY a1) (WrapY a2) = _y2 (get (Proxy :: Proxy s)) a1 a2

v1 :: String
v1 = bind
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y1 (WrapY 10 :: WrapY s Int)

v2 :: Int
v2 = bind
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y2 (WrapY 10 :: WrapY s Int) (WrapY 20 :: WrapY s Int)
