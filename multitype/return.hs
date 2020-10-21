{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds,
             DataKinds,
             GADTs,
             EmptyCase,
             InstanceSigs,
             OverloadedStrings,
             PolyKinds,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind
    ( Constraint
    , Type
    )
import Data.Singletons.Prelude (Elem)
import Data.Singletons.Sigma
    ( Sigma((:&:))
    , projSigma2
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X (s :: S) where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)


f1 :: Bool -> X 'S1 -> Either (X 'S2) (X 'S3)
f1 True x = Left $ X2 2
f1 False x = Right $ X3 "3"

c1 :: Text
c1 = case f1 True X1 of
         Left (X2 n) -> T.pack $ show n
         Right (X3 t) -> t


f2 :: Bool -> X 'S1 -> Sigma S (TyCon X)
f2 True x = SS2 :&: X2 2
f2 False x = SS3 :&: X3 "3"

c2 :: Text
c2 = projSigma2 p $ f2 True X1
  where
    p :: X s -> Text
    p X1 = undefined
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
    p (X4 _) = undefined


data F3 :: S -> Type where
    F32 :: X 'S2 -> F3 'S2
    F33 :: X 'S3 -> F3 'S3

data F3Sym0 :: S ~> Type
type instance Apply F3Sym0 x = F3 x

f3 :: Bool -> X 'S1 -> Sigma S F3Sym0
f3 True x = SS2 :&: F32 (X2 2)
f3 False x = SS3 :&: F33 (X3 "3")

c3 :: Text
c3 = projSigma2 p $ f3 True X1
  where
    p :: F3 s -> Text
    p (F32 (X2 n)) = T.pack $ show n
    p (F33 (X3 t)) = t


data SigmaP (s :: Type) (p :: s ~> Constraint) (t :: s ~> Type) where
    (:&?:) :: (p @@ fst) => Sing (fst :: s) -> t @@ fst -> SigmaP s p t

projSigmaP2 :: forall s p t r. (forall (fst :: s). p @@ fst => (t @@ fst) -> r) -> SigmaP s p t -> r
projSigmaP2 f ((_ :: Sing (fst :: s)) :&?: b) = f @fst b


type family F4 (x :: S) :: Constraint where
    F4 'S2 = ()
    F4 'S3 = ()
    F4 _ = ('True ~ 'False)

-- genDefunSymbols [''F4]
data F4Sym0 :: S ~> Constraint
type instance Apply F4Sym0 x = F4 x

f4 :: Bool -> X 'S1 -> SigmaP S F4Sym0 (TyCon X)
f4 True x = SS2 :&?: X2 2
f4 False x = SS3 :&?: X3 "3"

c4 :: Text
c4 = projSigmaP2 p $ f4 True X1
  where
    p :: F4 s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t

c4' :: Text
c4' = case f4 True X1 of
    SS2 :&?: X2 n -> T.pack $ show n
    SS3 :&?: X3 t -> t


type family OneOf l t :: Constraint where
    OneOf l t = If (Elem t l) (() :: Constraint) ('True ~ 'False)

-- genDefunSymbols [''OneOf]
data OneOfSym0 :: l ~> t ~> Constraint
type instance Apply OneOfSym0 x = OneOfSym1 x
data OneOfSym1 :: l -> t ~> Constraint
type instance Apply (OneOfSym1 l) t = OneOf l t
type OneOfSym2 l t = OneOf l t


f5 :: Bool -> X 'S1 -> SigmaP S (OneOfSym1 '[ 'S2, 'S3 ]) (TyCon X)
f5 True x = SS2 :&?: X2 2
f5 False x = SS3 :&?: X3 "3"

c5 :: Text
c5 = projSigmaP2 p $ f5 True X1
  where
    p :: OneOf '[ 'S2, 'S3 ] s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t


data SigmaL (l :: [s :: Type]) (t :: s ~> Type) where
    (:&!:) :: OneOf l fst => Sing (fst :: s) -> t @@ fst -> SigmaL l t

projSigmaL2 :: forall s (l :: [s]) t r. (forall (fst :: s). OneOf l fst => (t @@ fst) -> r) -> SigmaL l t -> r
projSigmaL2 f ((_ :: Sing (fst :: s)) :&!: b) = f @fst b

f6 :: Bool -> X 'S1 -> SigmaL '[ 'S2, 'S3 ] (TyCon X)
f6 True x = SS2 :&!: X2 2
f6 False x = SS3 :&!: X3 "3"

c6 :: Text
c6 = projSigmaL2 p $ f6 True X1
  where
    p :: OneOf '[ 'S2, 'S3 ] s => X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
