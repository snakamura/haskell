{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             LambdaCase,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons.Decide (Decision(Proved, Disproved))
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

data SomeX where
    SomeX :: Sing s -> X s -> SomeX


data F :: S -> Type where
    F2 :: F 'S2
    F3 :: F 'S3

f :: F s -> X s -> Text
f F2 (X2 n) = T.pack $ show n
f F3 (X3 t) = t


f1 :: SomeX -> Maybe Text
f1 = \case
    SomeX SS1 _ -> Nothing
    SomeX SS2 x -> Just $ f F2 x
    SomeX SS3 x -> Just $ f F3 x
    SomeX SS4 _ -> Nothing

c1 :: Maybe Text
c1 = f1 $ SomeX SS2 $ X2 2


isF :: Sing s -> Decision (F s)
isF SS1 = Disproved $ \case {}
isF SS2 = Proved F2
isF SS3 = Proved F3
isF SS4 = Disproved $ \case {}

f2 :: SomeX -> Maybe Text
f2 (SomeX s x) | Proved p <- isF s = Just $ f p x
               | otherwise = Nothing

c2 :: Maybe Text
c2 = f2 $ SomeX SS2 $ X2 2
