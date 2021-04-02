{-# LANGUAGE FlexibleInstances,
             FunctionalDependencies,
             GADTs,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             TypeApplications,
             TypeFamilies
#-}
{-# OPTIONS -Wall #-}

import Data.Text
import qualified Data.Text as T
import Text.Read (readMaybe)

type family R a where
    R Int = Text
    R Text = Maybe Int

class F a where
    f1 :: a -> R a
instance F Int where
    f1 = T.pack . show
instance F Text where
    f1 = readMaybe . T.unpack


data W a r where
    WInt :: W Int Text
    WText :: W Text (Maybe Int)

f2 :: W a r -> a -> r
f2 WInt = T.pack . show
f2 WText = readMaybe . T.unpack

class WC a r | a -> r where
    w :: W a r
instance WC Int Text where
    w = WInt
instance WC Text (Maybe Int) where
    w = WText

f3 :: forall a r. WC a r => a -> r
f3 x = case w @a of
         WInt -> T.pack $ show x
         WText -> readMaybe $ T.unpack x
