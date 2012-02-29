{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Exception
import Control.Monad.CatchIO as CatchIO
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Typeable

data TestException = TestException deriving (Show, Typeable)

instance Exception TestException

test f = runStateT go "Init"
    where
      go = do CatchIO.bracket s e (const f) `CatchIO.catch` c
              printState "Last"
              put "Last"
              return "Last"
      s = do printState "Start"
             put "Start"
      e _ = do printState "End"
               put "End"
      c (_ :: SomeException) = do printState "Catch"
                                  put "Catch"
                                  return "Catch"

printState at = get >>= \state -> liftIO $ print $ "At " ++ at ++ ": " ++ state

test1, test2 :: IO (String, String)
test1 = test $ do printState "Func"
                  put "Func"
                  return "Func"
test2 = test $ do printState "Func"
                  put "Func"
                  liftIO $ throwIO TestException
