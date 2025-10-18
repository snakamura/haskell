module Map2 where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import GHC.Base
import GHC.TypeLits

type HList :: (Type -> Type) -> [Type] -> Type
data HList f xs where
    HNil :: HList f '[]
    HCons :: f x -> HList f xs -> HList f (x ': xs)

infixr `HCons`

type Literal :: Symbol -> Type
newtype Literal n = Literal String

makeLiteral :: forall (n :: Symbol) -> KnownSymbol n => Literal n
makeLiteral n = Literal (symbolVal (Proxy @n))

makeLiteral' :: KnownSymbol n => Literal n
makeLiteral' @n = Literal (symbolVal (Proxy @n))

type Object :: Type -> Type
newtype Object n = Object { name :: n }

objects :: HList Object [Literal "a", Literal "b", Literal "c"]
objects = Object { name = makeLiteral "a" } `HCons`
          Object { name = makeLiteral "b" } `HCons`
          Object { name = makeLiteral "c" } `HCons`
          HNil

mapName :: HList Object xs -> HList Identity xs
mapName HNil = HNil
mapName (HCons x xs) = HCons (Identity (name x)) (mapName xs)

mappedNames :: HList Identity [Literal "a", Literal "b", Literal "c"]
mappedNames = mapName objects
