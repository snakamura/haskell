{-# LANGUAGE AllowAmbiguousTypes,
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
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.Sigma
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

f :: X 'S1 -> X 'S2
f x = X2 2


g1 :: Bool -> X 'S1 -> Either (X 'S2) (X 'S3)
g1 True x = Left $ X2 2
g1 False x = Right $ X3 "3"

c1 :: Text
c1 = case g1 True X1 of
         Left (X2 n) -> T.pack $ show n
         Right (X3 t) -> t


g2 :: Bool -> X 'S1 -> Sigma S (TyCon X)
g2 True x = SS2 :&: X2 2
g2 False x = SS3 :&: X3 "3"

c2 :: Text
c2 = projSigma2 p $ g2 True X1
  where
    p :: X s -> Text
    p X1 = undefined
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
    p (X4 _) = undefined


data Y (s :: S) where
    Y2 :: X 'S2 -> Y 'S2
    Y3 :: X 'S3 -> Y 'S3

singletons [d|
    type Z s = Y s
  |]

g3 :: Bool -> X 'S1 -> Sigma S ZSym0
g3 True x = SS2 :&: Y2 (X2 2)
g3 False x = SS3 :&: Y3 (X3 "3")

c3 :: Text
c3 = projSigma2 p $ g3 True X1
  where
    p :: Y s -> Text
    p (Y2 (X2 n)) = T.pack $ show n
    p (Y3 (X3 t)) = t


data SigmaP (s :: Type) (p :: s ~> Constraint) (t :: s ~> Type) where
    (:&?:) :: (p @@ fst) => Sing (fst :: s) -> t @@ fst -> SigmaP s p t

type family Y23 (x :: S) :: Constraint where
    Y23 S2 = ()
    Y23 S3 = ()
    Y23 _ = ('True ~ 'False)

data Y23Sym0 :: S ~> Constraint
type instance Apply Y23Sym0 x = Y23 x

projSigmaP2 :: forall s p t r. (forall (fst :: s). (t @@ fst) -> r) -> SigmaP s p t -> r
projSigmaP2 f ((_ :: Sing (fst :: s)) :&?: b) = f @fst b

{-
singletons [d|
    y23 :: S -> Constraint
    y23 S2 = ()
    y23 S3 = ()
  |]
-}

g4 :: Bool -> X 'S1 -> SigmaP S Y23Sym0 (TyCon X)
g4 True x = SS2 :&?: X2 2
g4 False x = SS3 :&?: X3 "3"

c4 :: Text
c4 = projSigmaP2 p $ g4 True X1
  where
    p :: X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t


data SigmaL (l :: [s :: Type]) (t :: s ~> Type) where
    (:&!:) :: OneOf fst l => Sing (fst :: s) -> t @@ fst -> SigmaL l t

type family OneOf t l :: Constraint where
    OneOf t l = If (Elem t l) (() :: Constraint) ('True ~ 'False)

projSigmaL2 :: forall s (l :: [s]) t r. (forall (fst :: s). (t @@ fst) -> r) -> SigmaL l t -> r
projSigmaL2 f ((_ :: Sing (fst :: s)) :&!: b) = f @fst b

g5 :: Bool -> X 'S1 -> SigmaL '[ 'S2, 'S3 ] (TyCon X)
g5 True x = SS2 :&!: X2 2
g5 False x = SS3 :&!: X3 "3"

c5 :: Text
c5 = projSigmaL2 p $ g5 True X1
  where
    p :: X s -> Text
    p (X2 n) = T.pack $ show n
    p (X3 t) = t
