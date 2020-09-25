module Door
    ( Door
    , name
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Text (Text)

data State = Opened | Closed | Locked Text deriving (Show, Eq)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)

makeLocked :: Text -> Text -> Door
makeLocked name key = Door name $ Locked key

open :: Door -> Maybe Door
open door@(Door _ Closed) = Just $ door { state = Opened }
open _ = Nothing

close :: Door -> Maybe Door
close door@(Door _ Opened) = Just $ door { state = Closed }
close _ = Nothing

lock :: Text -> Door -> Maybe Door
lock key door@(Door _ Closed) = Just $ door { state = Locked key }
lock _ _ = Nothing

unlock :: Text -> Door -> Maybe Door
unlock key door@(Door _ (Locked lockedKey))
    | lockedKey == key = Just $ door { state = Closed }
    | otherwise = Just door
unlock _ _ = Nothing

knock :: Door -> Maybe Door
knock door@(Door _ Closed) = Just door
knock door@(Door _ (Locked _)) = Just door
knock _ = Nothing
