{-# LANGUAGE DataKinds,
             FunctionalDependencies,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             TypeApplications,
             TypeFamilies
#-}
{-# OPTIONS -Wall #-}

import Data.Text (Text)
import qualified Data.Text as T

data S = S1 | S2 | S3

data X (s :: S) where
    X1 :: Int -> X 'S1
    X2 :: Text -> X 'S2
    X3 :: Int -> Text -> X 'S3


type family T (s :: S) where
    T 'S1 = Int
    T 'S2 = Text
    T 'S3 = Text

t1 :: X s -> T s
t1 (X1 n) = n
t1 (X2 t) = t
t1 (X3 n t) = T.pack (show n) <> t


data G (s :: S) r where
    G1 :: G 'S1 Int
    G2 :: G 'S2 Text
    G3 :: G 'S3 Text

g1 :: G s r -> X s -> r
g1 G1 (X1 n) = n
g1 G2 (X2 t) = t
g1 G3 (X3 n t) = T.pack (show n) <> t

class C (s :: S) r | s -> r where
    g :: G s r
instance C 'S1 Int where
    g = G1
instance C 'S2 Text where
    g = G2
instance C 'S3 Text where
    g = G3

g2 :: forall s r. C s r => X s -> r
g2 x = case (g @s @r, x) of
    (G1, (X1 n)) -> n
    (G2, (X2 t)) -> t
    (G3, (X3 n t)) -> T.pack (show n) <> t
