{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings
#-}

import Data.Either (fromRight)
import Data.Text (Text)
import Door

forceOpen :: Text -> SingState state -> Door state -> Maybe (Door 'Opened)
forceOpen key singState door =
    case singState of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     Right closedDoor -> Just $ open closedDoor
                     Left _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (SomeDoor singState door) = forceOpen key singState door

openedDoor :: Door 'Opened
openedDoor = open $ fromRight undefined . unlock "goodKey" $ makeLocked "opened" "goodKey"
closedDoor :: Door 'Closed
closedDoor = fromRight undefined $ unlock "goodKey" $ makeLocked "closed" "goodKey"
lockedDoor :: Door 'Locked
lockedDoor = makeLocked "locked" "goodKey"

doors :: [SomeDoor]
doors = [ SomeDoor SOpened openedDoor
        , SomeDoor SClosed closedDoor
        , SomeDoor SLocked lockedDoor
        ]

openedDoors :: [Maybe (Door 'Opened)]
openedDoors = map (forceOpenSomeDoor "goodKey") doors

notOpenedDoor :: Maybe (Door 'Opened)
notOpenedDoor = forceOpen "badKey" SLocked lockedDoor
