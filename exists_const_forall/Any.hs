module Any where

import Data.Kind

type Any :: Type
newtype Any = MkAny (forall a. a)

anyInt :: Any -> Int
anyInt (MkAny a) = a

anyValue :: Any
anyValue = MkAny undefined

type AnyC :: (Type -> Constraint) -> Type
newtype AnyC c = MkAnyC (forall a. (c a) => a)

anyCNum :: AnyC Num -> Int
anyCNum (MkAnyC a) = a

anyCNumValue :: AnyC Num
anyCNumValue = MkAnyC 1

anyCRead :: AnyC Read -> String
anyCRead (MkAnyC a) = let n = a :: Int in show n

anyCReadValue :: AnyC Read
anyCReadValue = MkAnyC (read "1")

type AnyF :: (k -> Type) -> Type
newtype AnyF f = MkAnyF (forall a. f a)

anyFLength :: AnyF [] -> Int
anyFLength (MkAnyF l) = let intList :: [Int] = l in length intList

anyFValue :: AnyF []
anyFValue = MkAnyF []

type data V = V1 | V2

type SV :: V -> Type
data SV v where
  MkSV1 :: Int -> SV V1
  MkSV2 :: String -> SV V2

type AnySV :: Type
newtype AnySV = MkAnySV (forall v. SV v)

anySVShow :: AnySV -> String
anySVShow (MkAnySV sv) =
  case sv of
    MkSV1 n -> show n
    MkSV2 s -> s

anySV :: AnySV
anySV = MkAnySV undefined
