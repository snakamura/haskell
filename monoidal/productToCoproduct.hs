{-# LANGUAGE FlexibleInstances,
             InstanceSigs,
             ScopedTypeVariables,
             UndecidableInstances
#-}

import Control.Applicative (Alternative(..))
import Data.Void (Void, absurd)

class Functor f => Choice f where
    choice :: (f a, f b) -> f (Either a b)
    none :: () -> f Void

instance Choice Maybe where
    choice :: (Maybe a, Maybe b) -> Maybe (Either a b)
    choice (Just a, _) = Just (Left a)
    choice (_, Just b) = Just (Right b)
    choice (_, _) = Nothing

    none :: () -> Maybe Void
    none () = Nothing

instance (Functor f, Choice f, Applicative f) => Alternative f where
    (<|>) :: f a -> f a -> f a
    x <|> y = either id id <$> choice (x, y)

    empty :: f a
    empty = absurd <$> none ()
