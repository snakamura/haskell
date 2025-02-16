{-# LANGUAGE TypeData #-}

module Reflection3 where

import Data.Coerce (coerce)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.Exts (withDict)
import Unsafe.Coerce (unsafeCoerce)

class Y a where
  y1 :: a -> String
  y2 :: a -> a -> Int

instance Y String where
  y1 = id
  y2 _ _ = 0

instance Y Int where
  y1 = show
  y2 = (+)

newtype WrappedString = WrappedString String

instance Y WrappedString where
  y1 (WrappedString s) = s <> "!!!"
  y2 _ _ = 100

type WrapY :: k -> Type -> Type
newtype WrapY s a = WrapY a

type DictY :: Type -> Type
data DictY a = DictY
  { _y1 :: a -> String,
    _y2 :: a -> a -> Int
  }

type BindY :: k -> Type -> Constraint
class BindY s a | s -> a where
  get :: Proxy s -> DictY (WrapY s a)

instance (BindY s a) => Y (WrapY s a) where
  y1 = _y1 $ get (Proxy :: Proxy s)
  y2 = _y2 $ get (Proxy :: Proxy s)

bindY :: forall s a r. DictY (WrapY s a) -> ((BindY s a) => Proxy s -> r) -> r
bindY dictY f =
  withDict
    @(BindY s a)
    {- @(Proxy s -> DictY (WrapY s a)) -}
    (const dictY)
    f
    (Proxy :: Proxy s)

type Magic :: k -> Type -> Type -> Type
newtype Magic s a r = Magic ((BindY s a) => Proxy s -> r)

unsafeBindY :: forall s a r. DictY (WrapY s a) -> ((BindY s a) => Proxy s -> r) -> r
unsafeBindY dictY f = unsafeCoerce (Magic f) (const dictY) (Proxy :: Proxy s)

v1 :: String
v1 = bindY
  DictY
    { _y1 = \(WrapY n) -> show $ n + 100,
      _y2 = \(WrapY n) (WrapY m) -> n * m
    }
  $ \(_ :: Proxy s) -> y1 (WrapY 10 :: WrapY s Int)

v2 :: Int
v2 = bindY
  DictY
    { _y1 = \(WrapY n) -> show $ n + 100,
      _y2 = \(WrapY n) (WrapY m) -> n * m
    }
  $ \(_ :: Proxy s) -> y2 (WrapY 10 :: WrapY s Int) (WrapY 20 :: WrapY s Int)

v3 :: String
v3 = bindY
  ( coerce
      DictY
        { _y1 = \n -> show $ n + 100,
          _y2 = (*)
        }
  )
  $ \(_ :: Proxy s) -> y1 (WrapY 10 :: WrapY s Int)

bindY' :: forall s a r. DictY a -> ((BindY s a) => Proxy s -> r) -> r
bindY' dictY f =
  withDict
    @(BindY s a)
    {- @(Proxy s -> DictY (WrapY s a)) -}
    (const (coerce {- @(DictY a) @(DictY (WrapY s a)) -} dictY))
    f
    (Proxy :: Proxy s)

unsafeBindY' :: forall s a r. DictY a -> ((BindY s a) => Proxy s -> r) -> r
unsafeBindY' dictY f =
  unsafeCoerce
    (Magic f)
    (const (coerce @(DictY a) @(DictY (WrapY s a)) dictY))
    (Proxy :: Proxy s)

v4 :: String
v4 = bindY'
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y1 (WrapY 10 :: WrapY s Int)

type data BadKind = Bad

bad :: WrapY Bad Int
bad = bindY'
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> WrapY 10 :: WrapY s Int

instance BindY Bad Int where
  get _ =
    DictY
      { _y1 = \(WrapY n) -> show $ n + 1000,
        _y2 = \(WrapY n) (WrapY m) -> n + m
      }

v5 :: String
v5 = y1 bad
