module Literal
  ( Literal(Literal),
    pattern Lit,
    getLiteral,
    Literal',
    pattern L',
    pattern Lit',
    makeLiteral',
  )
where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type Literal :: Symbol -> Type
data Literal s = Literal

getLiteral :: forall (s :: Symbol). (KnownSymbol s) => Literal s -> String
getLiteral Literal = symbolVal (Proxy @s)

pattern Lit :: forall (s :: Symbol). (KnownSymbol s) => String -> Literal s
pattern Lit s <- (getLiteral -> s)

{-# COMPLETE Lit #-}

type Literal' :: Symbol -> Type
newtype Literal' s = Literal' String deriving (Show, Eq)

makeLiteral' :: forall (s :: Symbol) -> (KnownSymbol s) => Literal' s
makeLiteral' s = Literal' (symbolVal (Proxy @s))

pattern Lit' :: forall (s :: Symbol). String -> Literal' s
pattern Lit' s <- Literal' s

{-# COMPLETE Lit' #-}

pattern L' :: forall (s :: Symbol). (KnownSymbol s) => Literal' s
pattern L' <- ((makeLiteral' s ==) -> True)
  where
    L' = makeLiteral' s

{-# COMPLETE L' #-}
