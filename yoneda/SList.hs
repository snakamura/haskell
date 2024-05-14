module SList where

import Prelude hiding (length)

data SList a = SCons !a !(SList a) | SNil deriving (Show, Functor)

from :: [a] -> SList a
from [] = SNil
from (x : xs) = SCons x (from xs)

length :: SList a -> Int
length SNil = 0
length (SCons _ xs) = 1 + length xs
