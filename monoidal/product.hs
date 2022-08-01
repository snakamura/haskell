{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

class Functor f => Monoidal f where
    op :: (f a, f b) -> f (a, b)
    unit :: f ()

instance Monoidal Maybe where
    op :: (Maybe a, Maybe b) -> Maybe (a, b)
    op (Just a, Just b) = Just (a, b)
    op _ = Nothing

    unit :: Maybe ()
    unit = Just ()

instance (Functor f, Monoidal f) => Applicative f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
    ff <*> fa = let fp :: f (a -> b, a) = op (ff, fa)
                in fmap (\(f, a) -> f a) fp

    pure :: a -> f a
    pure a = fmap (const a) unit
