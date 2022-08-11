{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Data.Void (Void)

class Functor f => Monoidal f where
    mu :: Either (f a) (f b) -> f (Either a b)
    epsilon :: Void -> f Void

instance Monoidal Maybe where
    mu :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
    mu (Left (Just a)) = Just (Left a)
    mu (Right (Just b)) = Just (Right b)
    mu _ = Nothing

    epsilon :: Void -> Maybe Void
    epsilon _ = Nothing
