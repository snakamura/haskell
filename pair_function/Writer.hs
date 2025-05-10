module Writer where

import Data.Bifunctor
import Control.Comonad

newtype Writer m a = Writer (m, a)

instance Functor (Writer m) where
  fmap :: (a -> b) -> Writer m a -> Writer m b
  fmap a2b (Writer (m, a)) = Writer (m, a2b a)

instance Monoid m => Applicative (Writer m) where
  pure :: a -> Writer m a
  pure a = Writer (mempty, a)

  (<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
  Writer (ma2b, a2b) <*> Writer (ma, a) = Writer (ma <> ma2b, a2b a)

instance Monoid m => Monad (Writer m) where
  (>>=) :: Writer m a -> (a -> Writer m b) -> Writer m b
  Writer (ma, a) >>= a2wb = let Writer (mb, b) = a2wb a in Writer (ma <> mb, b)

withWriter :: (String, Int)
withWriter =
  let w1 :: Int -> Writer String String
      w1 n = Writer ("1st\n", show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = Writer ("2nd\n", length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure
      Writer (w, a) = return 100 >>= w1 >>= w2 >>= w3
  in (w, a)


newtype Traced m a = Traced (m -> a)

instance Functor (Traced m) where
  fmap :: (a -> b) -> Traced m a -> Traced m b
  fmap a2b (Traced m2a) = Traced (a2b . m2a)

instance Monoid m => Comonad (Traced m) where
  extract :: (Traced m a) -> a
  extract (Traced m2a) = m2a mempty

  extend :: (Traced m a -> b) -> Traced m a -> Traced m b
  extend ta2b (Traced m2a) = Traced $ \mb -> ta2b (Traced $ \ma -> m2a (ma <> mb))

withTraced :: (String, Int)
withTraced =
  let t1 :: Traced String (String, Int) -> (String, String)
      t1 (Traced m2a) = second (show . (+ 1)) $ m2a "1st\n"
      t2 :: Traced String (String, String) -> (String, Int)
      t2 (Traced m2a) = second ((* 10) . length) $ m2a "2nd\n"
      t3 :: Traced String (String, Int) -> (String, Int)
      t3 = extract
      Traced w2a' = Traced (, 100) =>> t1 =>> t2 =>> t3
   in w2a' ""
