module Door
    ( Door
    , name
    , OpenedDoor
    , ClosedDoor
    , LockedDoor
    , makeLocked
    , open
    , close
    , lock
    , unlock
    ) where

import Data.Text (Text)

data Door = Door {
    name :: Text
} deriving (Show, Eq)

newtype OpenedDoor = OpenedDoor Door deriving (Show, Eq)
newtype ClosedDoor = ClosedDoor Door deriving (Show, Eq)
newtype LockedDoor = LockedDoor Door deriving (Show, Eq)

makeLocked :: Text -> LockedDoor
makeLocked = LockedDoor . Door

open :: ClosedDoor -> OpenedDoor
open (ClosedDoor door) = OpenedDoor door

close :: OpenedDoor -> ClosedDoor
close (OpenedDoor door) = ClosedDoor door

lock :: ClosedDoor -> LockedDoor
lock (ClosedDoor door) = LockedDoor door

unlock :: LockedDoor -> ClosedDoor
unlock (LockedDoor door) = ClosedDoor door
