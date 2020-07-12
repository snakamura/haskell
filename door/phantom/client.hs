{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             KindSignatures,
             OverloadedStrings
#-}

import Door

class ForceOpenDoor (state :: State) where
    forceOpen :: Door state -> Door 'Opened

instance ForceOpenDoor 'Opened where
    forceOpen = id
instance ForceOpenDoor 'Closed where
    forceOpen = open
instance ForceOpenDoor 'Locked where
    forceOpen = open . unlock

openedDoor :: Door 'Opened
openedDoor = open $ unlock $ makeDoor "opened"
closedDoor :: Door 'Closed
closedDoor = unlock $ makeDoor "closed"
lockedDoor :: Door 'Locked
lockedDoor = makeDoor "locked"

door1, door2, door3 :: Door 'Opened
door1 = forceOpen openedDoor
door2 = forceOpen closedDoor
door3 = forceOpen lockedDoor

data SomeDoor = forall state. ForceOpenDoor state => SomeDoor (Door state)

doors :: [SomeDoor]
doors = [SomeDoor openedDoor, SomeDoor closedDoor, SomeDoor lockedDoor]

openedDoors :: [Door 'Opened]
openedDoors = map (\(SomeDoor door) -> forceOpen door) doors
