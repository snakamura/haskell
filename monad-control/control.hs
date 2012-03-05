{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

testFunc1 :: (Int -> IO a) -> IO a
testFunc1 f = print "Test" >> f 1

func1 :: Int -> MaybeT IO Int
func1 n = do lift $ print n
             return n

test1 = runMaybeT go
    where
      go = control $ \run -> testFunc1 $ run . func1


testFunc0 :: IO a -> IO a
testFunc0 f = print "Test" >> f

func0 :: MaybeT IO Int
func0 = do lift $ print 0
           return 0

test0 = runMaybeT go
    where
      go = control $ \run -> testFunc0 $ run func0


testFunc2 :: (Int -> Int -> IO a) -> IO a
testFunc2 f = print "Test" >> f 2 3

func2 :: Int -> Int -> MaybeT IO Int
func2 m n = do lift $ print $ n + m
               return $ n + m

test2 = runMaybeT go
    where
      go = control $ \run -> testFunc2 $ \x y -> run $ func2 x y


func00 = do lift $ lift $ print 0
            return 0

test00 = runMaybeT $ runMaybeT go
    where
      go = control $ \run -> control $ \run2 -> testFunc0 $ run2 $ run func00


func0b = do get >>= liftBase . print
            return 0

test0b = runMaybeT $ runStateT go 10
    where
      go = control $ \run -> testFunc0 $ run func0b
