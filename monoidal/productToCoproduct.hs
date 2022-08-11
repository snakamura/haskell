{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Control.Applicative (Alternative(..))
import Data.Void (Void, absurd)

class Functor f => Monoidal f where
    mu :: (f a, f b) -> f (Either a b)
    epsilon :: () -> f Void

instance Monoidal Maybe where
    mu :: (Maybe a, Maybe b) -> Maybe (Either a b)
    mu (Just a, _) = Just (Left a)
    mu (_, Just b) = Just (Right b)
    mu (_, _) = Nothing

    epsilon :: () -> Maybe Void
    epsilon () = Nothing

instance (Functor f, Monoidal f, Applicative f) => Alternative f where
    (<|>) :: f a -> f a -> f a
    x <|> y = either id id <$> mu (x, y)

    empty :: f a
    empty = absurd <$> epsilon ()
