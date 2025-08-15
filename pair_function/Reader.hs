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

join :: Reader e (Reader e a) -> Reader e a
join (Reader e2ra) = Reader $ \e -> let Reader e2a = e2ra e in e2a e

ask :: Reader e e
ask = Reader id

local :: (e -> e) -> Reader e a -> Reader e a
local e2e (Reader e2a) = Reader $ e2a . e2e

withReader :: Int
withReader =
  let r1 :: Int -> Reader Int String
      r1 n = Reader $ show . (+ (n + 1))
      r2 :: String -> Reader Int Int
      r2 s = Reader (* (length s + 2))
      r3 :: Int -> Reader Int Int
      r3 n = Reader $ \e -> let Reader e'2a = Reader $ \e' -> e' + n in e'2a (e * 2)
      env = 10
      Reader e2a = pure 100 >>= r1 >>= r2 >>= r3
   in e2a env

withReader' :: Int
withReader' =
  let r1 :: Int -> Reader Int String
      r1 n = ask >>= \m -> pure $ show (m + n + 1)
      r2 :: String -> Reader Int Int
      r2 s = ask >>= \m -> pure $ m * (length s + 2)
      r3 :: Int -> Reader Int Int
      r3 n = local (* 2) $ ask >>= \e -> pure $ e + n
      env = 10
      Reader e2a = pure 100 >>= r1 >>= r2 >>= r3
   in e2a env
