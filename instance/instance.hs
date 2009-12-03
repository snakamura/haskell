{-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-}

class ToString a where
  toString :: a -> String

newtype Wrap a = Wrap a

instance Show a => ToString (Wrap a) where
  toString (Wrap x) = "Normal:" ++ show x

instance ToString (Wrap Int) where
  toString (Wrap x) = "Int:" ++ show x

f :: Num a => Wrap a -> String
f = toString
