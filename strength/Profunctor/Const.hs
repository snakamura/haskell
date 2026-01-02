module Profunctor.Const where

import Data.Profunctor (Profunctor (..))

newtype Const r a b = Const r

instance Profunctor (Const r) where
  dimap :: (s -> a) -> (b -> t) -> Const r a b -> Const r s t
  dimap _ _ (Const r) = Const r
