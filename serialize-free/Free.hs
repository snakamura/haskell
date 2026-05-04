module Free where

data Free f a = Pure a | Free (f (Free f a))

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure a) = Pure (f a)
    fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Pure

    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    Pure f <*> x = fmap f x
    Free g <*> x = Free (fmap (<*> x) g)

instance Functor f => Monad (Free f) where
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Pure a >>= f = f a
    Free x >>= f = Free (fmap (>>= f) x)

foldFree :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = pure a
foldFree f (Free as) = f as >>= foldFree f
