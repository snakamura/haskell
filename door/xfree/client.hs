{-# LANGUAGE OverloadedStrings,
             RebindableSyntax
#-}

import Control.XMonad ((>>:))
import Control.XMonad.Do ((>>))
import Data.String (fromString)
import Door
import Prelude hiding ((>>=), (>>))

runValid :: IO ()
runValid = run (makeLocked "valid") $ unlock >>: knock >>: open >>: close

{-
runInvalid :: IO ()
runInvalid = run (makeLocked "invalid") $ knock >>: open
-}

runValidDo :: IO ()
runValidDo = run (makeLocked "valid") $ do
    unlock
    knock
    open
    close
