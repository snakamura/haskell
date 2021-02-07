{-# LANGUAGE AllowAmbiguousTypes,
             RankNTypes,
             TypeApplications,
             TypeFamilies,
             TypeOperators
#-}

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind (Type)

type f ~> g = forall a. f a -> g a

maybeToList :: Maybe ~> []
maybeToList (Just a) = [a]
maybeToList Nothing = []

identityToMaybe :: Identity ~> Maybe
identityToMaybe (Identity a) = Just a

maybeMaybeToMaybe :: Compose Maybe Maybe ~> Maybe
maybeMaybeToMaybe (Compose (Just (Just a))) = Just a
maybeMaybeToMaybe _ = Nothing

class Functor m => Monoid' m where
    mempty' :: Identity a -> m a
    mappend' :: Compose m m a -> m a

instance Monoid' Maybe where
    mempty' = identityToMaybe
    mappend' = maybeMaybeToMaybe
