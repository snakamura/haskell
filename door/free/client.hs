{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromJust)
import Door

valid :: Maybe Door
valid = validate (makeLocked "valid") $ do
    unlock
    knock
    open
    close

invalid :: Maybe Door
invalid = validate (makeLocked "valid") $ do
    knock
    open

runValid :: IO ()
runValid = run (makeLocked "valid") $ do
    unlock
    knock
    open
    close

runInvalid :: IO ()
runInvalid = run (makeLocked "valid") $ do
    knock
    open
