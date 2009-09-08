{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

class Mul a b where
  type Result a b
  (*) :: a -> b -> Result a b

instance Mul Int Int where
  type Result Int Int = Int
  (*) = (Prelude.*)

instance Mul Int Double where
  type Result Int Double = Double
  (*) x y = fromIntegral x Prelude.* y
