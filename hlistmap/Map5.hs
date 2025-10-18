module Map5 where

import Data.Kind
import GHC.Base hiding (map)
import GHC.TypeLits
import Data.Proxy
import Prelude hiding (map)

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

type MapType :: [Type] -> [Type]
type family MapType xs where
    MapType '[] = '[]
    MapType (Object n ': xs) = n ': MapType xs

type Map :: [Type] -> Constraint
class Map xs where
    map :: (forall n. Object n -> n) -> HList xs -> HList (MapType xs)

instance Map '[] where
    map :: (forall n. Object n -> n) -> HList '[] -> HList '[]
    map _ HNil = HNil

instance Map xs => Map (Object n ': xs) where
    map :: (forall n'. Object n' -> n') -> HList (Object n ': xs) -> HList (n ': MapType xs)
    map f (HCons x xs) = HCons (f x) (map f xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name objects
