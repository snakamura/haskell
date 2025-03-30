module SomeC where

import Data.Kind

type SomeC :: (Type -> Constraint) -> Type
data SomeC c = forall a. (c a) => MkSomeC a

someCShowValue :: SomeC Show
someCShowValue = MkSomeC (1 :: Int)

fromSomeCShow :: SomeC Show -> String
fromSomeCShow (MkSomeC a) = show a

toSomeCShow :: Int -> SomeC Show
toSomeCShow n = MkSomeC n

forward :: (SomeC c -> a) -> (forall x. c x => x -> a)
forward g = \x -> g (MkSomeC x)

backward :: (forall x. c x => x -> a) -> (SomeC c -> a)
backward h = \(MkSomeC x) -> h x
