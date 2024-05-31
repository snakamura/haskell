module Item where

import Data.Functor.Identity

data Item n = Item
  { price :: n,
    internalPrice :: n
  }
  deriving (Show)

pickPrice, pickInternalPrice :: Item n -> Identity n
pickPrice (Item price _) = Identity price
pickInternalPrice (Item _ internalPrice) = Identity internalPrice
