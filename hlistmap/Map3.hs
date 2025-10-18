module Map3 where

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
    mapName :: (forall n. Object (Literal n) -> Literal n) -> HList xs -> HList (MapToName xs)

instance MapName '[] where
    mapName :: (forall n. Object (Literal n) -> Literal n) -> HList '[] -> HList '[]
    mapName _ HNil = HNil

instance MapName xs => MapName (Object (Literal n) ': xs) where
    mapName :: (forall n'. Object (Literal n') -> Literal n') -> HList (Object (Literal n) ': xs) -> HList (Literal n ': MapToName xs)
    mapName f (HCons x xs) = HCons (f x) (mapName f xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapName name objects
