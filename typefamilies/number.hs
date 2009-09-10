{-# LANGUAGE TypeFamilies, EmptyDataDecls, UndecidableInstances, ScopedTypeVariables #-}

data Zero
data Succ a

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five

type family Add a b
type instance Add Zero a = a
type instance Add (Succ a) b = Succ (Add a b)

type family Mul a b
type instance Mul Zero a = Zero
type instance Mul (Succ a) b = Add b (Mul a b)

type family Fac a
type instance Fac Zero = One
type instance Fac (Succ a) = Mul (Succ a) (Fac a)

class N a where
  n :: a -> Int

instance N Zero where
  n _ = 0

instance N a => N (Succ a) where
  n _ = 1 + n (undefined :: a)
