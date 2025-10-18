module Map7 where

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

data IsObject n where
    IsObject :: IsObject (Object n)

data AreObjects ns where
    AreObjectsNil :: AreObjects '[]
    AreObjectsCons :: IsObject n -> AreObjects ns -> AreObjects (n ': ns)

class IsObjectC n where
    isObject :: n -> IsObject n

instance IsObjectC (Object n) where
    isObject :: Object n -> IsObject (Object n)
    isObject _ = IsObject

class AreObjectsC ns where
    areObjects :: HList ns -> AreObjects ns

instance AreObjectsC '[] where
    areObjects :: HList '[] -> AreObjects '[]
    areObjects HNil = AreObjectsNil

instance (IsObjectC (Object n), AreObjectsC ns) => AreObjectsC (Object n ': ns) where
    areObjects :: HList (Object n ': ns) -> AreObjects (Object n ': ns)
    areObjects (HCons x xs) = AreObjectsCons (isObject x) (areObjects xs)

map :: AreObjects xs -> (forall n. Object n -> n) -> HList xs -> HList (MapType xs)
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons IsObject o) f (HCons x xs) = HCons (f x) (map o f xs)

mapC :: AreObjectsC xs => (forall n. Object n -> n) -> HList xs -> HList (MapType xs)
mapC f xs = map (areObjects xs) f xs

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapC name objects
