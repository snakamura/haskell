module Reflection2 where

import GHC.Exts (withDict)
import Unsafe.Coerce (unsafeCoerce)

class X a where
  x :: a -> String

instance X String where
  x = id

instance X Int where
  x = show

invoke :: (X a) => a -> String
invoke a = x a

v1 :: String
v1 = invoke (100 :: Int) <> "!!!"

invokeCPS :: (X a) => a -> (a -> r) -> r
invokeCPS a f = f a

v2 :: String
v2 = invokeCPS (100 :: Int) $ \n -> x n <> "!!!"

newtype Magic a r = Magic ((X a) => a -> r)

unsafeInvoke :: a -> ((X a) => a -> r) -> r
unsafeInvoke a f = unsafeCoerce (Magic f) (\n -> show (n + 1 :: Int)) a

v3 :: String
v3 = unsafeInvoke (100 :: Int) $ \n -> x n <> "!!!"

unsafeInvoke2 :: (a -> String) -> a -> ((X a) => a -> r) -> r
unsafeInvoke2 x' a f = unsafeCoerce (Magic f) x' a

v4 :: String
v4 = unsafeInvoke2 (\n -> show (n + 2)) (100 :: Int) $ \n -> x n <> "!!!"

safeInvoke :: forall a r. (a -> String) -> a -> ((X a) => a -> r) -> r
safeInvoke x' a f = withDict @(X a) x' f a

v5 :: String
v5 = safeInvoke (\n -> show (n + 3)) (100 :: Int) $ \n -> x n <> "!!!"
