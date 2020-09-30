{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings,
             PatternSynonyms,
             RebindableSyntax
#-}

import Control.XApplicative (xpure)
import Control.XFreer (XFree)
import Control.XMonad ((>>:))
import Control.XMonad.Do
    ( (>>)
    , (>>=)
    )
import Data.Singletons (pattern FromSing)
import Data.String (fromString)
import Door
import Prelude hiding ((>>=), (>>))

runValidDo :: IO ()
runValidDo = run (makeClosed "valid") actions

actions :: XFree Action 'Closed 'Locked ()
actions = do
    lock "key"
    r <- tryUnlocking "key"
    case r of
        FromSing r -> do
            commitUnlocking r
            case r of
                SUnlocked -> do
                    knock
                    open
                    close
                    lock "key"
                SNotUnlocked -> pure ()
