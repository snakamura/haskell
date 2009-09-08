{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

class (Result a b ~ c) => Mul a b c where
  type Result a b
  (*) :: a -> b -> Result a b

