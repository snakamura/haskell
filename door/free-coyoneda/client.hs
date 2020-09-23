{-# LANGUAGE OverloadedStrings #-}

import Door

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
