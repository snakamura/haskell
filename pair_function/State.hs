module State where

newtype State t a = State (t -> (t, a))

instance Functor (State t) where
  fmap :: (a -> b) -> State t a -> State t b
  fmap a2b (State t2ta) = State $ \t -> let (t', a) = t2ta t in (t', a2b a)

instance Applicative (State t) where
  pure :: a -> State t a
  pure a = State (, a)

  (<*>) :: State t (a -> b) -> State t a -> State t b
  State t2ta2b <*> State t2ta = State $ \t -> let (t', a2b) = t2ta2b t
                                                  (t'', a) = t2ta t'
                                               in (t'', a2b a)

instance Monad (State t) where
  (>>=) :: State t a -> (a -> State t b) -> State t b
  State t2ta >>= a2sb = State $ \t -> let (t', a) = t2ta t
                                          State t2tb = a2sb a
                                       in t2tb t'

get :: State t t
get = State $ \t -> (t, t)

put :: t -> State t ()
put t = State $ \_ -> (t, ())

modify :: (t -> t) -> State t ()
modify f = State $ \t -> (f t, ())

withState :: (String, Int)
withState =
  let s1 :: Int -> State String String
      s1 n = State $ \t -> (t <> "!!!", t <> show (n + 10))
      s2 :: String -> State String Int
      s2 s = State $ \t -> (t <> s, length s)
      s3 :: Int -> State String Int
      s3 = pure
      state = "state"
      State t2ta = pure 100 >>= s1 >>= s2 >>= s3
   in t2ta state

withState' :: (String, Int)
withState' =
  let s1 :: Int -> State String String
      s1 n = get >>= \t -> put (t <> "!!!") >> pure (t <> show (n + 10))
      s2 :: String -> State String Int
      s2 s = modify (<> s) >> pure (length s)
      s3 :: Int -> State String Int
      s3 = pure
      state = "state"
      State t2ta = pure 100 >>= s1 >>= s2 >>= s3
   in t2ta state
