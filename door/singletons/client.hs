{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             GADTs,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeApplications
#-}

import Data.Singletons
    ( SingI
    , TyCon
    , sing
    )
import Data.Singletons.Sigma (Sigma((:&:)))
import Door

forceOpen :: forall state. SingI state => Door state -> Door 'Opened
forceOpen door =
    case sing @state of
        SOpened -> door
        SClosed -> open $ knock door
        SLocked -> open $ unlock $ knock door

forceOpen2 :: Sigma State (TyCon Door) -> Door 'Opened
forceOpen2 (SOpened :&: openedDoor) = openedDoor
forceOpen2 (SClosed :&: closedDoor) = open $ knock closedDoor
forceOpen2 (SLocked :&: lockedDoor) = open $ unlock $ knock lockedDoor

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

doors :: [SomeDoor]
doors = [ SomeDoor openedDoor
        , SomeDoor closedDoor
        , SomeDoor lockedDoor
        ]

openedDoors :: [Door 'Opened]
openedDoors = map (\(SomeDoor door) -> forceOpen door) doors

doors2 :: [Sigma State (TyCon Door)]
doors2 = [ SOpened :&: openedDoor
         , SClosed :&: closedDoor
         , SLocked :&: lockedDoor
         ]

openedDoors2 :: [Door 'Opened]
openedDoors2 = map forceOpen2 doors2
