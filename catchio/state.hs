{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}

import Control.Exception (Exception)
import Control.Monad.CatchIO (MonadCatchIO, bracket, catch, throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Typeable (Typeable)
import Prelude hiding (catch)

data TestException = TestException deriving (Show, Typeable)

instance Exception TestException

test :: MonadCatchIO m => StateT String m String -> m (String, String)
test f = runStateT go "Init"
    where
      go = do bracket (updateState "Start")
                      (const $ updateState "End")
                      (const f)
                  `catch` \(_ :: TestException) -> updateState "Catch"
              updateState "Last"

updateState :: (MonadIO m, MonadState String m) => String -> m String
updateState at = do printState at
                    put at
                    return at

printState :: (MonadIO m, MonadState String m) => String -> m ()
printState at = get >>= \state -> liftIO $ print $ "At " ++ at ++ ": " ++ state

test1, test2 :: IO (String, String)
test1 = test $ updateState "Func"
test2 = test $ do updateState "Func"
                  throw TestException
