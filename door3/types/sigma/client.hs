{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeApplications
#-}

import Data.Singletons (TyCon)
import Data.Singletons.Sigma (Sigma((:&:)))
import Data.Text (Text)
import Door
import Sigma (OneOfSym1, SigmaP((:&?:)))

forceOpen :: Text -> SState state -> Door state -> Maybe (Door 'Opened)
forceOpen key state door =
    case state of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     SClosed :&?: closedDoor -> Just $ open closedDoor
                     SLocked :&?: _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (state :&: door) = forceOpen key state door

openedDoor :: Door 'Opened
openedDoor = open $ toClosedDoor . unlock "goodKey" $ makeLocked "opened" "goodKey"
closedDoor :: Door 'Closed
closedDoor = toClosedDoor $ unlock "goodKey" $ makeLocked "closed" "goodKey"
lockedDoor :: Door 'Locked
lockedDoor = makeLocked "locked" "goodKey"

toClosedDoor :: SigmaP State (OneOfSym1 '[ 'Closed, 'Locked ]) (TyCon Door) -> Door 'Closed
toClosedDoor (SClosed :&?: closedDoor) = closedDoor
toClosedDoor _ = error "Must not happen"

doors :: [SomeDoor]
doors = [ SOpened :&: openedDoor
        , SClosed :&: closedDoor
        , SLocked :&: lockedDoor
        ]

openedDoors :: [Maybe (Door 'Opened)]
openedDoors = map (forceOpenSomeDoor "goodKey") doors

notOpenedDoor :: Maybe (Door 'Opened)
notOpenedDoor = forceOpen "badKey" SLocked lockedDoor
