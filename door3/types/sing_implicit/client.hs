{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings,
             ScopedTypeVariables,
             TypeApplications
#-}

import Data.Either (fromRight)
import Data.Text (Text)
import Door

forceOpen :: forall state. SingStateI state => Text -> Door state -> Maybe (Door 'Opened)
forceOpen key door =
    case singState @state of
        SOpened -> Just door
        SClosed -> Just $ open $ knock door
        SLocked -> case unlock key $ knock door of
                     Right closedDoor -> Just $ open closedDoor
                     Left _ -> Nothing

forceOpenSomeDoor :: Text -> SomeDoor -> Maybe (Door 'Opened)
forceOpenSomeDoor key (SomeDoor door) = forceOpen key door

openedDoor :: Door 'Opened
openedDoor = open $ fromRight undefined . unlock "goodKey" $ makeLocked "opened" "goodKey"
closedDoor :: Door 'Closed
closedDoor = fromRight undefined $ unlock "goodKey" $ makeLocked "closed" "goodKey"
lockedDoor :: Door 'Locked
lockedDoor = makeLocked "locked" "goodKey"

doors :: [SomeDoor]
doors = [ SomeDoor openedDoor
        , SomeDoor closedDoor
        , SomeDoor lockedDoor
        ]

openedDoors :: [Maybe (Door 'Opened)]
openedDoors = map (forceOpenSomeDoor "goodKey") doors

notOpenedDoor :: Maybe (Door 'Opened)
notOpenedDoor = forceOpen "badKey" lockedDoor
