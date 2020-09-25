{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Door

openedByUnlocking, openedByUnlockingTwice :: Maybe Door
openedByUnlocking = (open <=< unlock "goodKey" <=< knock) $ makeLocked "locked" "goodKey"
openedByUnlockingTwice = (open <=< unlock "goodKey" <=< unlock "badKey" <=< knock) $ makeLocked "locked" "goodKey"

notOpened1, notOpened2 :: Maybe Door
notOpened1 = open $ makeLocked "locked" "goodKey"
notOpened2 = (open <=< unlock "badKey") $ makeLocked "locked" "goodKey"
