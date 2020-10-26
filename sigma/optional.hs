{-# LANGUAGE DataKinds,
             DeriveFunctor,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Data.Kind (Type)
import Data.Singletons ( SLambda(SLambda))
import Data.Singletons.Prelude
    ( FlipSym1
    , Id
    , IdSym0
    , sId
    )
import Data.Singletons.Sigma
    ( Sigma((:&:))
    , mapSigma
    , projSigma2
    )
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

singletons [d|
    data O = S | N deriving (Show, Eq)
  |]

data Optional (o :: O) (a :: Type) where
    Some :: a -> Optional 'S a
    None :: Optional 'N a

deriving instance Show a => Show (Optional o a)

textToInt :: Text -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
textToInt t | Just n <- readMaybe $ T.unpack t = SS :&: Some n
            | otherwise = SN :&: None

addText :: Int -> Text -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
addText n t = projSigma2 f (textToInt t)
  where
    f :: Optional o Int -> Sigma O (FlipSym1 (TyCon Optional) @@ Int)
    f (Some m) = SS :&: Some (n + m)
    f None = SN :&: None

type Opt a = Sigma O (FlipSym1 (TyCon Optional) @@ a)

singletons [d|
    g :: O -> O
    g S = S
    g N = N
  |]

addText' :: Int -> Text -> Opt Int
addText' n t = mapSigma (SLambda sG :: SLambda GSym0) f (textToInt t)
  where
    f :: Optional o Int -> Optional (G o) Int
    f (Some m) = Some $ n + m
    f None = None

addText'' :: Int -> Text -> Opt Int
addText'' n t = mapSigma (SLambda sId :: SLambda (IdSym0 :: O ~> O)) f (textToInt t)
  where
    f :: Optional o Int -> Optional (Id o) Int
    f (Some m) = Some $ n + m
    f None = None

deriving instance Functor (Optional o)

addText''' :: Int -> Text -> Opt Int
addText''' n t = mapSigma (SLambda sId :: SLambda (IdSym0 :: O ~> O))
                          (fmap (+ n) :: Optional o Int -> Optional (Id o) Int)
                          (textToInt t)
