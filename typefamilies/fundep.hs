{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

class Mul a b c | a b -> c where
  (*) :: a -> b -> c

instance Mul Int Int Int where
  (*) = (Prelude.*)

instance Mul Int Double Double where
  (*) x y = fromIntegral x Prelude.* y
