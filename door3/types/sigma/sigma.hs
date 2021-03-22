{-# LANGUAGE AllowAmbiguousTypes,
             ConstraintKinds,
             DataKinds,
             GADTs,
             KindSignatures,
             PolyKinds,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

module Sigma
    ( SigmaP((:&?:))
    , projSigmaP2
    , OneOf
    , OneOfSym0
    , OneOfSym1
    ) where

import Data.Kind (Constraint, Type)
import Data.Singletons.TH

data SigmaP (s :: Type) (p :: s ~> Constraint) (t :: s ~> Type) where
    (:&?:) :: (p @@ fst) => Sing (fst :: s) -> t @@ fst -> SigmaP s p t

projSigmaP2 :: forall s p t r. (forall (fst :: s). p @@ fst => (t @@ fst) -> r) -> SigmaP s p t -> r
projSigmaP2 f ((_ :: Sing (fst :: s)) :&?: b) = f @fst b


type family OneOf l t :: Constraint where
    OneOf l t = If (Elem t l) (() :: Constraint) ('True ~ 'False)

genDefunSymbols [''OneOf]
