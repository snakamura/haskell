{-# LANGUAGE DeriveFunctor,
             FlexibleInstances,
             InstanceSigs,
             NoImplicitPrelude,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Control.Applicative (Applicative(..))
import Data.Function (const)
import Data.Functor (Functor(..))
import Text.Show (Show)

class Functor f => Monoidal f where
    op :: (f a, f b) -> f (a, b)
    unit :: f ()

data Maybe a = Just a | Nothing deriving (Show, Functor)

instance Monoidal Maybe where
    op :: (Maybe a, Maybe b) -> Maybe (a, b)
    op (Just a, Just b) = Just (a, b)
    op (Just _, Nothing) = Nothing
    op (Nothing, _) = Nothing

    unit :: Maybe ()
    unit = Just ()

instance (Functor f, Monoidal f) => Applicative f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
    ff <*> fa = let fp :: f (a -> b, a) = op (ff, fa)
                in fmap (\(f, a) -> f a) fp

    pure :: a -> f a
    pure a = fmap (const a) unit
