module Door
    ( OpenedDoor
    , ClosedDoor
    , LockedDoor
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Text (Text)

data Door = Door {
    name :: Text
} deriving (Show, Eq)

data OpenedDoor = OpenedDoor Door deriving (Show, Eq)
data ClosedDoor = ClosedDoor Door deriving (Show, Eq)
data LockedDoor = LockedDoor Door Text deriving (Show, Eq)

makeLocked :: Text -> Text -> LockedDoor
makeLocked name key = LockedDoor (Door name) key

open :: ClosedDoor -> OpenedDoor
open (ClosedDoor door) = OpenedDoor door

close :: OpenedDoor -> ClosedDoor
close (OpenedDoor door) = ClosedDoor door

lock :: Text -> ClosedDoor -> LockedDoor
lock key (ClosedDoor door) = LockedDoor door key

unlock :: Text -> LockedDoor -> Either LockedDoor ClosedDoor
unlock key lockedDoor@(LockedDoor door lockedKey)
    | key == lockedKey = Right $ ClosedDoor door
    | otherwise = Left lockedDoor

class KnockableDoor door where
    knock :: door -> door
    knock = id
instance KnockableDoor ClosedDoor
instance KnockableDoor LockedDoor
