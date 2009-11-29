import Data.List
import Data.Monoid
import Data.Ord

{-
instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  GT `mappend` _ = GT
  EQ `mappend` o = o
-}

f = sortBy (comparing length `mappend` compare)
           (words "here is a bunch of words to sort first by length and then alphabetically")
