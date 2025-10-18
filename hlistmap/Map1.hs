module Map1 where

import Data.Kind
import GHC.Base
import GHC.TypeLits
import Data.Proxy

type HList :: [Type] -> Type
data HList xs where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

infixr `HCons`

type Literal :: Symbol -> Type
newtype Literal n = Literal String

makeLiteral :: forall (n :: Symbol) -> KnownSymbol n => Literal n
makeLiteral n = Literal (symbolVal (Proxy @n))

makeLiteral' :: KnownSymbol n => Literal n
makeLiteral' @n = Literal (symbolVal (Proxy @n))

type Object :: Type -> Type
newtype Object n = Object { name :: n }

objects :: HList [Object (Literal "a"), Object (Literal "b"), Object (Literal "c")]
objects = Object { name = makeLiteral "a" } `HCons`
          Object { name = makeLiteral "b" } `HCons`
          Object { name = makeLiteral "c" } `HCons`
          HNil

type MapToName :: [Type] -> [Type]
type family MapToName xs where
    MapToName '[] = '[]
    MapToName (Object (Literal n) ': xs) = Literal n ': MapToName xs

type MapName :: [Type] -> Constraint
class MapName xs where
    mapName :: HList xs -> HList (MapToName xs)

instance MapName '[] where
    mapName :: HList '[] -> HList '[]
    mapName HNil = HNil

instance MapName xs => MapName (Object (Literal n) ': xs) where
    mapName :: HList (Object (Literal n) ': xs) -> HList (Literal n ': MapToName xs)
    mapName (HCons x xs) = HCons (name x) (mapName xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapName objects
