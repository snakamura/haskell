module Reflection1 where

import Data.Proxy
import Data.Reflection

newtype ReflectedEq s a = ReflectedEq a

reflectEq :: Proxy s -> a -> ReflectedEq s a
reflectEq _ a = ReflectedEq a

unreflectEq :: ReflectedEq s a -> a
unreflectEq (ReflectedEq a) = a

newtype ReifiedEq a = ReifiedEq
  { _eq :: (a -> a -> Bool)
  }

instance (Reifies s (ReifiedEq a)) => Eq (ReflectedEq s a) where
  ReflectedEq x == ReflectedEq y =
    let ReifiedEq {..} = reflect (Proxy :: Proxy s)
     in _eq x y

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq e l = reify (ReifiedEq eq) $ \proxy ->
  elem (reflectEq proxy e) $ map (reflectEq proxy) l

data WrapEq a = WrapEq a (a -> a -> Bool)

instance Eq (WrapEq a) where
  (WrapEq lhs eq) == (WrapEq rhs _) = eq lhs rhs

elemBy' :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy' eq e l = elem (WrapEq e eq) $ map (`WrapEq` eq) l
