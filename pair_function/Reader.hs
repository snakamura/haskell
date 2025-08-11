module Reader where

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
  fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap a2b (Reader e2a) = Reader $ a2b . e2a

instance Applicative (Reader e) where
  pure :: a -> Reader e a
  pure a = Reader $ const a

  (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
  Reader e2a2b <*> Reader e2a = Reader $ \e -> e2a2b e (e2a e)

instance Monad (Reader e) where
  (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  Reader e2a >>= a2rb = Reader $ \e -> let Reader e2b = a2rb (e2a e) in e2b e

withReader :: Int
withReader =
  let r1 :: Int -> Reader Int String
      r1 n = Reader $ show . (+ (n + 1))
      r2 :: String -> Reader Int Int
      r2 s = Reader (* (length s + 2))
      r3 :: Int -> Reader Int Int
      r3 = pure
      env = 10
      Reader e2a = pure 100 >>= r1 >>= r2 >>= r3
   in e2a env
