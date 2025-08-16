module Writer where

newtype Writer m a = Writer (m, a)

instance Functor (Writer m) where
  fmap :: (a -> b) -> Writer m a -> Writer m b
  fmap a2b (Writer (m, a)) = Writer (m, a2b a)

instance (Monoid m) => Applicative (Writer m) where
  pure :: a -> Writer m a
  pure a = Writer (mempty, a)

  (<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
  Writer (ma2b, a2b) <*> Writer (ma, a) = Writer (ma <> ma2b, a2b a)

instance (Monoid m) => Monad (Writer m) where
  (>>=) :: Writer m a -> (a -> Writer m b) -> Writer m b
  Writer (ma, a) >>= a2wb = let Writer (mb, b) = a2wb a in Writer (ma <> mb, b)

join :: (Monoid m) => Writer m (Writer m a) -> Writer m a
join (Writer (mwa, Writer (ma ,a))) = Writer (mwa <> ma, a)

tell :: m -> Writer m ()
tell m = Writer (m, ())

withWriter :: (String, Int)
withWriter =
  let w1 :: Int -> Writer String String
      w1 n = Writer ("1st\n", show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = Writer ("2nd\n", length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure

      initialValue = 100
      Writer (w, a) = return initialValue >>= w1 >>= w2 >>= w3
   in (w, a)

withWriter' :: (String, Int)
withWriter' =
  let w1 :: Int -> Writer String String
      w1 n = tell "1st\n" >> pure (show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = tell "2nd\n" >> pure (length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure

      initialValue = 100
      Writer (w, a) = return initialValue >>= w1 >>= w2 >>= w3
   in (w, a)

withWriter'' :: (String, Int)
withWriter'' =
  let w1 :: Int -> Writer String String
      w1 n = Writer ("1st\n", show $ n + 1)
      w2 :: String -> Writer String Int
      w2 s = Writer ("2nd\n", length s * 10)
      w3 :: Int -> Writer String Int
      w3 = pure

      initialValue = 100
      Writer (w, a) = join $ join $ fmap (fmap w3) $ fmap w2 $ w1 initialValue
   in (w, a)
