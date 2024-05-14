module NList where

import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (length)

data NList a = NCons a (NList a) | NNil deriving (Show, Generic)

instance (NFData a) => NFData (NList a)

from :: [a] -> NList a
from [] = NNil
from (x : xs) = NCons x (from xs)

to :: NList a -> [a]
to NNil = []
to (NCons x xs) = x : to xs

length :: NList a -> Int
length NNil = 0
length (NCons _ xs) = 1 + length xs
