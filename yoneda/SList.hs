module SList where

import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (length)

data SList a = SCons !a !(SList a) | SNil deriving (Show, Functor, Generic)

instance (NFData a) => NFData (SList a)

from :: [a] -> SList a
from [] = SNil
from (x : xs) = SCons x (from xs)

length :: SList a -> Int
length SNil = 0
length (SCons _ xs) = 1 + length xs
