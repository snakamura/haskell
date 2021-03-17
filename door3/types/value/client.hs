{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Door

forceOpen :: Text -> Door -> Maybe Door
forceOpen key door =
    case knock door of
      Just door ->
          case unlock key door of
            Just (Right closedDoor) ->
                case open closedDoor of
                  Just openedDoor -> Just openedDoor
                  Nothing -> error "Must not happen"
            Just (Left _) -> Nothing
            Nothing ->
                case open door of
                  Just openedDoor -> Just openedDoor
                  Nothing -> error "Must not happen"
      Nothing -> Just door

openedDoor :: Door
openedDoor = fromJust $ (open <=< fmap (fromRight undefined) . unlock "goodKey") $ makeLocked "opened" "goodKey"
closedDoor :: Door
closedDoor = fromJust $ fmap (fromRight undefined) . unlock "goodKey" $ makeLocked "closed" "goodKey"
lockedDoor :: Door
lockedDoor = makeLocked "locked" "goodKey"

doors :: [Door]
doors = [openedDoor, closedDoor, lockedDoor]

openedDoors :: [Maybe Door]
openedDoors = map (forceOpen "goodKey") doors

notOpenedDoor :: Maybe Door
notOpenedDoor = forceOpen "badKey" lockedDoor
