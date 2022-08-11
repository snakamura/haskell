{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

class Functor f => Monoidal f where
    mu :: (f a, f b) -> f (a, b)
    epsilon :: () -> f ()

instance Monoidal Maybe where
    mu :: (Maybe a, Maybe b) -> Maybe (a, b)
    mu (Just a, Just b) = Just (a, b)
    mu _ = Nothing

    epsilon :: () -> Maybe ()
    epsilon () = Just ()

instance (Functor f, Monoidal f) => Applicative f where
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
    ff <*> fa = let fp :: f (a -> b, a) = mu (ff, fa)
                in fmap (\(f, a) -> f a) fp

    pure :: a -> f a
    pure a = fmap (const a) (epsilon ())
