module SomeC where

import Data.Kind

type SomeC :: (Type -> Constraint) -> Type
data SomeC c where
  MkSomeC :: (c a) => a -> SomeC c

someCShow :: SomeC Show -> String
someCShow (MkSomeC a) = show a

someCShowValue :: SomeC Show
someCShowValue = MkSomeC (1 :: Int)
