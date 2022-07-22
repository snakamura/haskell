module Stringify where

import Base
import Add

class Stringify a where
    stringify :: a -> String

instance Stringify Literal where
    stringify (Literal n) = show n

instance (Stringify a, Stringify b) => Stringify (Add a b) where
    stringify (Add a b) = stringify a <> stringify b
