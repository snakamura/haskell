{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception
import Control.Monad.CatchIO as CatchIO
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Typeable

data TestException = TestException deriving (Show, Typeable)

instance Exception TestException

{-
instance MonadCatchIO m => MonadCatchIO (MaybeT m) where
    m `catch` f = mapMaybeT (\m' -> m' `CatchIO.catch` (\e -> runMaybeT $ f e)) m
    block = mapMaybeT CatchIO.block
    unblock = mapMaybeT CatchIO.unblock
-}

test :: MonadIO m => MaybeT m a -> m (Maybe a)
test f = runMaybeT $ do liftIO $ print "Start"
                        v <- f
                        liftIO $ print "End"
                        return v

testBracket :: MonadCatchIO m => MaybeT m a -> m (Maybe a)
testBracket f = runMaybeT $ CatchIO.bracket (liftIO $ print "Start")
                                            (const $ liftIO $ print "End")
                                            (const f)


test1, test2, test3, test4, test5, test6 :: IO (Maybe Int)
test1 = test $ return 1
test2 = test empty
test3 = test $ liftIO $ throwIO TestException
test4 = testBracket $ return 1
test5 = testBracket empty
test6 = testBracket $ liftIO $ throwIO TestException
