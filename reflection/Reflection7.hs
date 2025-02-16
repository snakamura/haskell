module Reflection7 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies (..), reify)

class Y a where
  y1 :: a -> String
  y2 :: a -> a -> Int

instance Y String where
  y1 = id
  y2 _ _ = 0

instance Y Int where
  y1 = show
  y2 = (+)

type Wrap :: k -> Type -> Type
newtype Wrap s a = Wrap a

type DictY :: Type -> Type
data DictY a = DictY
  { _y1 :: a -> String,
    _y2 :: a -> a -> Int
  }

instance (Reifies s (DictY a)) => Y (Wrap s a) where
  y1 (Wrap a) = _y1 (reflect (Proxy :: Proxy s)) a
  y2 (Wrap a1) (Wrap a2) = _y2 (reflect (Proxy :: Proxy s)) a1 a2

v1 :: String
v1 = reify
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y1 (Wrap 10 :: Wrap s Int)

v2 :: Int
v2 = reify
  DictY
    { _y1 = \n -> show $ n + 100,
      _y2 = (*)
    }
  $ \(_ :: Proxy s) -> y2 (Wrap 10 :: Wrap s Int) (Wrap 20 :: Wrap s Int)
