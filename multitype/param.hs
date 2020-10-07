{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind
import Data.Singletons.TH
import Data.Text (Text)

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X (s :: S) where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)

f :: X 'S2 -> X 'S1
f x = X1

g1 :: Either (X 'S2) (X 'S3) -> X S1
g1 (Left (X2 _)) = X1
g1 (Right (X3 _)) = X1

c1 :: X 'S1
c1 = g1 $ Left $ X2 2


type family G2 s :: Constraint where
    G2 'S2 = ()
    G2 'S3 = ()
    G2 _ = ('True ~ 'False)

g2 :: G2 s => X s -> X 'S1
g2 (X2 _) = X1
g2 (X3 _) = X1

c2 :: X 'S1
c2 = g2 $ X2 2


data G3 :: S -> Type where
    G32 :: G3 'S2
    G33 :: G3 'S3

g3 :: G3 s -> X s -> X 'S1
g3 _ X1 = error "Error"
g3 _ (X2 _) = X1
g3 _ (X3 _) = X1
g3 _ (X4 _) = error "Error"

c3, c3' :: X 'S1
c3 = g3 G32 $ X2 2
c3' = g3 undefined $ X4 4
