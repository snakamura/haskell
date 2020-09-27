{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings
#-}

import Data.Singletons.Sigma (projSigma2)
import Data.Text (Text)
import Door

openedByUnlocking, openedByUnlockingTwice :: Door 'Opened
openedByUnlocking = projSigma2 openUnlocked $ unlock "goodKey" $ knock $ makeLocked "locked" "goodKey"
    where
        openUnlocked :: MaybeUnlocked state -> Door 'Opened
        openUnlocked (Unlocked door@(ClosedDoor _)) = open door
openedByUnlockingTwice = projSigma2 unlockLocked $ unlock "badKey" $ knock $ makeLocked "locked" "goodKey"
    where
        unlockLocked :: MaybeUnlocked state -> Door 'Opened
        unlockLocked (NotUnlocked door@(LockedDoor _ _)) = projSigma2 openUnlocked $ unlock "goodKey" door
        openUnlocked :: MaybeUnlocked state -> Door 'Opened
        openUnlocked (Unlocked door@(ClosedDoor _)) = open door

tryOpen :: [Text] -> Maybe (Door 'Opened)
tryOpen keys = try keys $ knock $ makeLocked "locked" "goodKey"
    where
        try [] _ = Nothing
        try (key:keys) door = projSigma2 try' $ unlock key door
            where
                try' :: (MaybeUnlockedDoor state) -> Maybe (Door 'Opened)
                try' (Unlocked door@(ClosedDoor _)) = Just $ open door
                try' (NotUnlocked door@(LockedDoor _ _)) = try keys door
