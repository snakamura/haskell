{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables #-}

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Typeable
import Prelude hiding (catch)

data TestException = TestException deriving (Show, Typeable)

instance Exception TestException

test = runMaybeT $ catchMaybeT (liftIO $ throwIO TestException)
                               (\e -> return 0)

-- catchMaybeT m f = mapMaybeT (\m' -> m' `catch` (\(e :: SomeException) -> runMaybeT $ f e)) m
catchMaybeT m f = MaybeT $ runMaybeT m `catch` (\(e :: SomeException) -> runMaybeT $ f e)
