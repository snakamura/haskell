{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind

data Nat = Z | S Nat deriving (Show)

type One = S Z

type Two = S One

data Vec :: Nat -> * -> * where
  Nil :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a

infixr 5 :>

f :: (l ~ Two) => Vec l a -> a
f (e :> _) = e

type Gt :: Nat -> Nat -> Constraint
type family Gt n m where
  Gt Z _ = 'True ~ 'False
  Gt _ Z = ()
  Gt (S n) (S m) = Gt n m

g :: (Gt l Two) => Vec l a -> a
g (e :> _) = e

g' :: Vec (S (S (S n))) a -> a
g' (e :> _) = e
