{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             GADTs,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeApplications
#-}

import Door

forceOpen :: forall state. SingStateI state => Door state -> Door 'Opened
forceOpen door =
    case singState @state of
        SOpened -> door
        SClosed -> open door
        SLocked -> open $ unlock door

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

doors :: [SomeDoor]
doors = [SomeDoor openedDoor, SomeDoor closedDoor, SomeDoor lockedDoor]

openedDoors :: [Door 'Opened]
openedDoors = map (\(SomeDoor door) -> forceOpen door) doors
