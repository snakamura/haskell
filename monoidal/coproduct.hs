{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Data.Void (Void)

class Functor f => Monoidal f where
    op :: Either (f a) (f b) -> f (Either a b)
    unit :: f Void

instance Monoidal Maybe where
    op :: Either (Maybe a) (Maybe b) -> Maybe (Either a b)
    op (Left (Just a)) = Just (Left a)
    op (Right (Just b)) = Just (Right b)
    op _ = Nothing

    unit :: Maybe Void
    unit = Nothing
