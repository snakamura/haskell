module Door
    ( Door
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Text (Text)

data State = Opened | Closed | Locked deriving (Show, Eq, Ord, Enum, Bounded)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)

makeLocked :: Text -> Door
makeLocked name = Door name Locked

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

knock :: Door -> Maybe Door
knock door@(Door name Closed) = Just door
knock door@(Door name Locked) = Just door
knock _ = Nothing
