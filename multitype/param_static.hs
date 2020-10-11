{-# LANGUAGE DataKinds,
             EmptyCase,
             FlexibleContexts,
             GADTs,
             InstanceSigs,
             KindSignatures,
             MultiParamTypeClasses,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind
    ( Constraint
    , Type
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T

singletons [d|
    data S = S1 | S2 | S3 | S4 deriving (Show, Eq)
  |]

data X :: S -> Type where
    X1 :: X 'S1
    X2 :: Int -> X 'S2
    X3 :: Text -> X 'S3
    X4 :: Float -> X 'S4

deriving instance Show (X s)


f1 :: Either (X 'S2) (X 'S3) -> Text
f1 (Left (X2 n)) = T.pack $ show n
f1 (Right (X3 t)) = t

c1 :: Text
c1 = f1 $ Left $ X2 2


type family F2 s :: Constraint where
    F2 'S2 = ()
    F2 'S3 = ()
    F2 _ = ('True ~ 'False)

f2 :: F2 s => X s -> Text
f2 (X2 n) = T.pack $ show n
f2 (X3 t) = t

c2 :: Text
c2 = f2 $ X2 2


data F3 :: S -> Type where
    F32 :: F3 'S2
    F33 :: F3 'S3

f3 :: F3 s -> X s -> Text
f3 F32 (X2 n) = T.pack $ show n
f3 F33 (X3 t) = t

c3 :: Text
c3 = f3 F32 $ X2 2


data F4 :: S -> Type where
    F42 :: F4 'S2
    F43 :: F4 'S3

class Proved p a where
    auto :: p a

instance Proved F4 'S2 where
    auto = F42

instance Proved F4 'S3 where
    auto = F43

f4 :: Proved F4 s => X s -> Text
f4 = f4' auto
  where
    f4' :: F4 s -> X s -> Text
    f4' F42 (X2 n) = T.pack $ show n
    f4' F43 (X3 t) = t

c4 :: Text
c4 = f4 $ X2 2
