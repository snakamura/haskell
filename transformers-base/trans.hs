{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Base
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.STRef

test = runMaybeT (runMaybeT go)
    where
      go = do liftBase $ print "test"
              return 3

test2 = runMaybeT go
    where
      go = liftBase $ return 3

test3 = runST go
    where
      go = do r <- newSTRef 0
              runMaybeT $ runStateT (go2 r) 0
              readSTRef r
      go2 r = do liftBase $ writeSTRef r 10
                 return 3
