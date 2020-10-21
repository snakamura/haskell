{-# LANGUAGE DataKinds,
             GADTs,
             PolyKinds,
             TemplateHaskell,
             TypeFamilies,
             TypeOperators
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons.TH

singletons [d|
    data S = S1 | S2 | S3 | S4
  |]

data X (s :: S) = X

data SomeX where
    SomeX :: Sing s -> X s -> SomeX

data Some1 (t :: S -> Type) where
    Some1 :: Sing s -> t s -> Some1 t

data Some2 (t :: k -> Type) where
    Some2 :: Sing s -> t s -> Some2 t

data Some3 k (t :: k -> Type) where
    Some3 :: Sing s -> t s -> Some3 k t

data Some4 k (t :: k ~> Type) where
    Some4 :: Sing s -> t @@ s -> Some4 k t

data Some5 s (t :: s ~> Type) where
    Some5 :: Sing (fst :: s) -> t @@ fst -> Some5 s t
