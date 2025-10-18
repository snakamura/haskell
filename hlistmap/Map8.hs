module Map8 where

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

type MapItem :: Type -> Constraint
class MapItem o where
    type Mapped o :: Type
    mapItem :: (forall n. Object n -> n) -> o -> Mapped o

instance MapItem (Object n) where
    type Mapped (Object n) = n
    mapItem :: (forall n'. Object n' -> n') -> Object n -> n
    mapItem f = f

type MapAll :: [Type] -> [Type]
type family MapAll xs where
    MapAll '[] = '[]
    MapAll (x ': xs) = Mapped x ': MapAll xs

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c xs where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)

map :: All MapItem xs => (forall n. Object n -> n) -> HList xs -> HList (MapAll xs)
map _ HNil = HNil
map f (HCons x xs) = HCons (mapItem f x) (map f xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name objects
