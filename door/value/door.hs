module Door
    ( Door
    , makeDoor
    , name
    , open
    , close
    , lock
    , unlock
) where

import Data.Text (Text)

data State = Opened | Closed | Locked deriving (Show, Eq, Ord, Enum, Bounded)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)

makeDoor :: Text -> Door
makeDoor name = Door name Locked

open :: Door -> Maybe Door
open (Door name Closed) = Just $ Door name Opened
open _ = Nothing

close :: Door -> Maybe Door
close (Door name Opened) = Just $ Door name Closed
close _ = Nothing

lock :: Door -> Maybe Door
lock (Door name Closed) = Just $ Door name Locked
lock _ = Nothing

unlock :: Door -> Maybe Door
unlock (Door name Locked) = Just $ Door name Closed
unlock _ = Nothing
