module Literal
  ( Literal,
    makeLiteral,
    makeLiteral',
  )
where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type Literal :: Symbol -> Type
newtype Literal s = Literal String

makeLiteral :: forall (s :: Symbol) -> (KnownSymbol s) => Literal s
makeLiteral s = Literal (symbolVal (Proxy @s))

makeLiteral' :: (KnownSymbol s) => Literal s
makeLiteral' @s = Literal (symbolVal (Proxy @s))
