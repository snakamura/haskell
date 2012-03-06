{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

testFunc1 :: (Int -> IO a) -> IO a
testFunc1 f = print "Test" >> f 1

func1 :: Int -> StateT Int IO Int
func1 n = do get >>= lift . print
             put 1
             return n

test1 = runStateT go 0
    where
      go = do get >>= lift . print
              liftFunc1 testFunc1 func1
              get >>= lift . print

liftFunc1 :: ((a -> IO (b, s)) -> IO (b, s)) -> (a -> StateT s IO b) -> StateT s IO b
liftFunc1 f g = let h s x = runStateT (g x) s
                    z s = f (h s)
                in StateT $ \s -> z s


testFunc0 :: IO a -> IO a
testFunc0 f = print "Test" >> f

func0 :: StateT Int IO Int
func0 = do get >>= lift . print
           put 1
           return 0

test0 = runStateT go 0
    where
      go = do get >>= lift . print
              liftFunc0 testFunc0 func0
              get >>= lift . print

liftFunc0 :: (IO (b, s) -> IO (b, s)) -> StateT s IO b -> StateT s IO b
liftFunc0 f g = let h s = runStateT g s
                    z s = f (h s)
                in StateT $ \s -> z s


testFunc2 :: (Int -> Int -> IO a) -> IO a
testFunc2 f = print "Test" >> f 2 3

func2 :: Int -> Int -> StateT Int IO Int
func2 m n = do lift $ print $ n + m
               put $ n + m
               return $ n + m

test2 = runStateT go 0
    where
      go = liftFunc2 testFunc2 func2

liftFunc2 :: ((a -> b -> IO (c, s)) -> IO (c, s)) -> (a -> b -> StateT s IO c) -> StateT s IO c
liftFunc2 f g = let h s x y = runStateT (g x y) s
                    z s = f (h s)
                in StateT $ \s -> z s


test' = runStateT go 0
    where
      go = liftFunc' testFunc2 $ \x y -> runStateT $ func2 x y

-- liftFunc' :: (a -> b -> IO (c, s)) -> IO (c, s)) -> (a -> b -> StateT s IO c) -> StateT s IO c
liftFunc' f g = let h s = g s
                    z s = f (h s)
                in StateT $ \s -> z s


test'' = runStateT go 0
    where
      go = liftFunc'' testFunc2 $ \run x y -> run $ func2 x y

-- liftFunc'' :: (a -> b -> IO (c, s)) -> IO (c, s)) -> (a -> b -> StateT s IO c) -> StateT s IO c
liftFunc'' f g = let h s = (g runStateT) s
                     z s = f (h s)
                 in StateT $ \s -> z s


test''' = runStateT go 0
    where
      go = liftFunc''' $ \run -> testFunc2 $ \x y -> run (func2 x y)

-- liftFunc''' :: ((a -> b -> IO (c, s)) -> IO (c, s)) -> (a -> b -> StateT s IO c) -> StateT s IO c
liftFunc''' f = let z s = f (flip runStateT s)
                in StateT $ \s -> z s


test'''' = runStateT go 0
    where
      go = liftFunc'''' $ \run -> testFunc2 $ \x y -> run (func2 x y)

-- liftFunc''' :: ((a -> b -> IO (c, s)) -> IO (c, s)) -> (a -> b -> StateT s IO c) -> StateT s IO c
liftFunc'''' f = let z s = f (liftM P . flip runStateT s)
                  in StateT $ \s -> liftM unP $ z s

class MonadTransControl t where
    type V t :: * -> *
    liftFunc :: Monad m => ((t m a -> m (V t a)) -> m (V t b)) -> t m b

newtype P s a = P { unP :: (a, s) } deriving Show

instance MonadTransControl (StateT s) where
    type V (StateT s) = P s
--    liftFunc f = StateT $ \s -> do P (a, s') <- f (liftM P . flip runStateT s)
--                                   return (a, s')
    liftFunc f = StateT $ \s -> liftM unP $ f (liftM P . flip runStateT s)

t2 = runStateT go 0
    where
      go = do liftFunc $ \run -> testFunc2 $ \x y -> run (func2 x y)
              get >>= lift . print

{-
class MonadTransControl t where
    type V t :: * -> *
    liftFunc :: Monad m => ((t m a -> m (V t a)) -> m (V t b)) -> t m (V t b)
    restore :: Monad m => m (V t b) -> t m b

newtype P s a = P { unP :: (a, s) } deriving Show

instance MonadTransControl (StateT s) where
    type V (StateT s) = P s
    liftFunc f = StateT $ \s -> liftM (\a -> (a, s)) $ f (liftM P . flip runStateT s)
    restore m = StateT $ \_ -> liftM unP m

t2 = runStateT go 0
    where
      go = do n <- liftFunc $ \run -> testFunc2 $ \x y -> run (func2 x y)
              put 3
              m <- restore $ return n
              lift $ print m
-}
