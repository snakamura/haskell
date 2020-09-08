{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             GADTs,
             OverloadedStrings
#-}

import Door

forceOpen :: SingState state -> Door state -> Door 'Opened
forceOpen singDoor door =
    case singDoor of
        SOpened -> door
        SClosed -> open door
        SLocked -> open $ unlock door

openedDoor :: Door 'Opened
openedDoor = open $ unlock $ makeLocked "opened"
closedDoor :: Door 'Closed
closedDoor = unlock $ makeLocked "closed"
lockedDoor :: Door 'Locked
lockedDoor = makeLocked "locked"

door1, door2, door3 :: Door 'Opened
door1 = forceOpen SOpened openedDoor
door2 = forceOpen SClosed closedDoor
door3 = forceOpen SLocked lockedDoor

doors :: [SomeDoor]
doors = [ SomeDoor SOpened openedDoor
        , SomeDoor SClosed closedDoor
        , SomeDoor SLocked lockedDoor
        ]

openedDoors :: [Door 'Opened]
openedDoors = map (\(SomeDoor singState door) -> forceOpen singState door) doors
