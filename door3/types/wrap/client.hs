{-# LANGUAGE OverloadedStrings #-}

import Data.Either (fromRight)
import Data.Text (Text)
import Door

class ForceOpenableDoor door where
    forceOpen :: Text -> door -> Maybe OpenedDoor

instance ForceOpenableDoor OpenedDoor where
    forceOpen _ = Just
instance ForceOpenableDoor ClosedDoor where
    forceOpen _ = Just . open . knock
instance ForceOpenableDoor LockedDoor where
    forceOpen key lockedDoor
        | Right closedDoor <- unlock key (knock lockedDoor) = Just $ open closedDoor
        | otherwise = Nothing

openedDoor :: OpenedDoor
openedDoor = open $ fromRight undefined . unlock "goodKey" $ makeLocked "opened" "goodKey"
closedDoor :: ClosedDoor
closedDoor = fromRight undefined $ unlock "goodKey" $ makeLocked "closed" "goodKey"
lockedDoor :: LockedDoor
lockedDoor = makeLocked "locked" "goodKey"

openedDoors :: [Maybe OpenedDoor]
openedDoors = [ forceOpen "goodKey" openedDoor
              , forceOpen "goodKey" closedDoor
              , forceOpen "goodKey" lockedDoor
              ]

notOpenedDoor :: Maybe OpenedDoor
notOpenedDoor = forceOpen "badKey" lockedDoor
