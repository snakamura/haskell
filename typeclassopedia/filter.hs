{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Prelude hiding (filter)

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f (Just x) | f x = Just x
filterMaybe _ _              = Nothing

filter :: (Foldable f, Applicative t, Monoid (t a)) => (a -> Bool) -> f a -> t a
filter f = foldMap (\x -> if f x then pure x else mempty)

data X = X
       | Y
  deriving (Show, Eq)

deriving instance Functor First
deriving instance Applicative First


a = filterMaybe (== "1") $ Just "1"
a' = filterMaybe (== "1") $ Just "2"
b = filter (== "1") $ Just "1" :: Maybe String
b' = filter (== "1") $ Just "2" :: Maybe String
c = getFirst $ filter (== X) $ Just X
c' = getFirst $ filter (== X) $ Just Y
