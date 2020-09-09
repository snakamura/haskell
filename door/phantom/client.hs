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
    forceOpen = open . knock
instance ForceOpenDoor 'Locked where
    forceOpen = open . unlock . knock

openedDoor :: Door 'Opened
openedDoor = open $ unlock $ makeLocked "opened"
closedDoor :: Door 'Closed
closedDoor = unlock $ makeLocked "closed"
lockedDoor :: Door 'Locked
lockedDoor = makeLocked "locked"

door1, door2, door3 :: Door 'Opened
door1 = forceOpen openedDoor
door2 = forceOpen closedDoor
door3 = forceOpen lockedDoor

data SomeDoor = forall state. ForceOpenDoor state => SomeDoor (Door state)

doors :: [SomeDoor]
doors = [ SomeDoor openedDoor
        , SomeDoor closedDoor
        , SomeDoor lockedDoor
        ]

openedDoors :: [Door 'Opened]
openedDoors = map (\(SomeDoor door) -> forceOpen door) doors
