{-# LANGUAGE ExistentialQuantification,
             OverloadedStrings
#-}

import Door

class ForceOpenDoor door where
    forceOpen :: door -> OpenedDoor

instance ForceOpenDoor OpenedDoor where
    forceOpen = id
instance ForceOpenDoor ClosedDoor where
    forceOpen = open
instance ForceOpenDoor LockedDoor where
    forceOpen = open . unlock

openedDoor :: OpenedDoor
openedDoor = open $ unlock $ makeLocked "opened"
closedDoor :: ClosedDoor
closedDoor = unlock $ makeLocked "closed"
lockedDoor :: LockedDoor
lockedDoor = makeLocked "locked"

door1, door2, door3 :: OpenedDoor
door1 = forceOpen openedDoor
door2 = forceOpen closedDoor
door3 = forceOpen lockedDoor

data SomeDoor = forall door. ForceOpenDoor door => SomeDoor door

doors :: [SomeDoor]
doors = [ SomeDoor openedDoor
        , SomeDoor closedDoor
        , SomeDoor lockedDoor
        ]

openedDoors :: [OpenedDoor]
openedDoors = map (\(SomeDoor door) -> forceOpen door) doors
