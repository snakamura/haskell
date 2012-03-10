{-# LANGUAGE TypeFamilies, RankNTypes #-}

import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.State (StateT(StateT), runStateT, put)
import Data.Tuple (swap)


class MonadTrans t => MonadTransFunc t where
    type C t :: * -> *
    liftFunc :: Monad m => ((forall a. t m a -> m (C t a)) -> m (C t b)) -> t m b


instance MonadTransFunc MaybeT where
    type C MaybeT = Maybe
    liftFunc f = MaybeT $ f runMaybeT

{-
newtype P s a = P { unP :: (a, s) }

instance MonadTransFunc (StateT s) where
    type C (StateT s) = P s
    liftFunc f = StateT $ \s -> liftM unP $ f (liftM P . flip runStateT s)
-}

instance MonadTransFunc (StateT s) where
    type C (StateT s) = (,) s
    liftFunc f = StateT $ \s -> liftM swap $ f (liftM swap . flip runStateT s)


testFunc0 :: IO a -> IO a
testFunc0 f = print "testFunc0" >> f

testFunc1 :: (Int -> IO a) -> IO a
testFunc1 f = print "testFunc1" >> f 1

maybeFunc0 :: MaybeT IO Int
maybeFunc0 = do lift $ print "maybeFunc0"
                return 0

stateFunc1 :: Int -> StateT Int IO Int
stateFunc1 n = do lift $ print $ "stateFunc1: " ++ show n
                  put n
                  return n

maybe0 = runMaybeT go
    where
      go = liftFunc $ \run -> testFunc0 $ run $ maybeFunc0

state1 = runStateT go 0
    where
      go = liftFunc $ \run -> testFunc1 $ \x -> run $ stateFunc1 x

maybeStateFunc1 :: Int -> MaybeT (StateT Int IO) Int
maybeStateFunc1 n = do lift $ lift $ print $ "maybeStateFunc1: " ++ show n
                       lift $ put n
                       return n

maybeState1 = runStateT (runMaybeT go) 0
    where
      go = liftFunc $ \runMaybe ->
               liftFunc $ \runState ->
                   testFunc1 $ \x -> runState $ runMaybe $ maybeStateFunc1 x
