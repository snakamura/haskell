{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Door

forceOpen :: Door -> Door
forceOpen door = case unlock door of
                   Just closedDoor -> case open closedDoor of
                                        Just openedDoor -> openedDoor
                                        Nothing -> error "Must not happen"
                   Nothing -> case open door of
                                Just openedDoor -> openedDoor
                                Nothing -> door

openedDoor :: Door
openedDoor = fromJust $ (open <=< unlock) $ makeLocked "opened"
closedDoor :: Door
closedDoor = fromJust $ unlock $ makeLocked "closed"
lockedDoor :: Door
lockedDoor = makeLocked "locked"

door1, door2, door3 :: Door
door1 = forceOpen openedDoor
door2 = forceOpen closedDoor
door3 = forceOpen lockedDoor

doors :: [Door]
doors = [openedDoor, closedDoor, lockedDoor]

openedDoors :: [Door]
openedDoors = map forceOpen doors
