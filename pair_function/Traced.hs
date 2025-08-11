module Traced where

import Control.Comonad
import Data.Monoid

newtype Traced m a = Traced (m -> a)

instance Functor (Traced m) where
  fmap :: (a -> b) -> Traced m a -> Traced m b
  fmap a2b (Traced m2a) = Traced (a2b . m2a)

instance Monoid m => Comonad (Traced m) where
  extract :: (Traced m a) -> a
  extract (Traced m2a) = m2a mempty

  extend :: (Traced m a -> b) -> Traced m a -> Traced m b
  extend ta2b (Traced m2a) = Traced $ \mb -> ta2b (Traced $ \ma -> m2a (ma <> mb))

withTraced :: Double
withTraced =
  let original :: Traced (Sum Int) Double
      original = Traced $ \(Sum n) -> sin (fromIntegral n)
      gain :: Double -> Traced (Sum Int) Double -> Double
      gain g (Traced m2a) = g * m2a (Sum 0)
      delay :: Int -> Traced (Sum Int) Double -> Double
      delay d (Traced m2a) = m2a (Sum (-d))
      identity :: Traced (Sum Int) Double -> Double
      identity = extract
   in extract $ original =>> gain 2 =>> delay 3 =>> identity
