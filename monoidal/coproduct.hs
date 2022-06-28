{-# LANGUAGE DeriveFunctor,
             FlexibleInstances,
             InstanceSigs,
             NoImplicitPrelude,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Data.Either (Either(..))
import Data.Functor (Functor(..))
import Data.Void (Void)
import Text.Show (Show)

class Functor f => Monoidal f where
    op :: Either (f a) (f b) -> f (Either a b)
    unit :: f Void

data Maybe a = Just a | Nothing deriving (Show, Functor)

instance Monoidal Maybe where
    op :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
    op (Left (Just a)) = Just (Left a)
    op (Right (Just b)) = Just (Right b)
    op _ = Nothing

    unit :: Maybe Void
    unit = Nothing
