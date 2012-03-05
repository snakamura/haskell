{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, RankNTypes #-}

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

testFunc1 :: (Int -> IO a) -> IO a
testFunc1 f = print "Test" >> f 1

func1 :: Int -> MaybeT IO Int
func1 n = do lift $ print n
             return n

test1 = runMaybeT go
    where
      go = liftFunc1 testFunc1 func1

{-
-- liftFunc_ :: ((a -> IO b) -> IO b) -> (a -> MaybeT IO b) -> MaybeT IO b
liftFunc_ f g = let h x = liftM fromJust $ runMaybeT $ g x
                    z = f h
                in MaybeT $ Just <$> z
-}

liftFunc1 :: ((a -> IO (Maybe b)) -> IO (Maybe b)) -> (a -> MaybeT IO b) -> MaybeT IO b
liftFunc1 f g = let h x = runMaybeT $ g x
                    z = f h
                in MaybeT z


testFunc0 :: IO a -> IO a
testFunc0 f = print "Test" >> f

func0 :: MaybeT IO Int
func0 = do lift $ print 0
           return 0

test0 = runMaybeT go
    where
      go = liftFunc0 testFunc0 func0

liftFunc0 :: (IO (Maybe c) -> IO (Maybe c)) -> MaybeT IO c -> MaybeT IO c
liftFunc0 f g = let h = runMaybeT g
                    z = f h
                in MaybeT z


testFunc2 :: (Int -> Int -> IO a) -> IO a
testFunc2 f = print "Test" >> f 2 3

func2 :: Int -> Int -> MaybeT IO Int
func2 m n = do lift $ print $ n + m
               return $ n + m

test2 = runMaybeT go
    where
      go = liftFunc2 testFunc2 func2

liftFunc2 :: ((a -> b -> IO (Maybe c)) -> IO (Maybe c)) -> (a -> b -> MaybeT IO c) -> MaybeT IO c
liftFunc2 f g = let h x y = runMaybeT $ g x y
                    z = f h
                in MaybeT z


test2' = runMaybeT go
    where
      go = liftFunc2' testFunc2 (\x y -> runMaybeT $ func2 x y)

liftFunc2' :: (a -> IO (Maybe b)) -> a -> MaybeT IO b
liftFunc2' f g = let z = f g
                 in MaybeT z


test2'' = runMaybeT go
    where
      go = liftFunc2'' testFunc2 (\run x y -> run $ func2 x y)

liftFunc2'' :: (a -> IO (Maybe b)) -> ((MaybeT IO c -> IO (Maybe c)) -> a) -> MaybeT IO b
liftFunc2'' f g = let z = f (g runMaybeT)
                  in MaybeT z


test2''' = runMaybeT go
    where
      go = liftFunc2''' (\run -> testFunc2 (\x y -> run $ func2 x y))

liftFunc2''' :: ((MaybeT IO a -> IO (Maybe a)) -> IO (Maybe b)) -> MaybeT IO b
liftFunc2''' f = let z = f runMaybeT
                 in MaybeT z


class MonadTrans t => MonadTransControl t where
    type V t :: * -> *
    liftFunc :: Monad m => ((forall a. t m a -> m (V t a)) -> m (V t b)) -> t m b


instance MonadTransControl MaybeT where
    type V MaybeT = Maybe
    liftFunc f = MaybeT $ f runMaybeT

{-
newtype M a = M { unM :: Maybe a } deriving Show

instance MonadTransControl MaybeT where
    type V MaybeT = M
    liftFunc f = MaybeT $ liftM unM $ f $ liftM M . runMaybeT
-}

t0 = runMaybeT go
    where
      go = liftFunc $ \run -> testFunc0 $ run $ func0

t1 = runMaybeT go
    where
      go = liftFunc $ \run -> testFunc1 $ \x -> run $ func1 x

t2 = runMaybeT go
    where
      go = liftFunc $ \run -> testFunc2 $ \x y -> run $ func2 x y


func00 = do lift $ lift $ print 0
            return 0

tt0 = runMaybeT $ runMaybeT go
    where
      go = liftFunc $ \run -> liftFunc $ \run2 -> testFunc0 $ run2 $ run $ func00


testFunc10 :: (Int -> IO a) -> IO b -> IO a
testFunc10 f m = print "Test" >> m >> f 1

func0' :: MaybeT IO Char
func0' = do lift $ print 0
            -- return 'c'
            mzero

t10 = runMaybeT go
    where
      go = liftFunc $ \run -> testFunc10 (\x -> run $ func1 x) (run func0')
