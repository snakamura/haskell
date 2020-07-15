{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , name
    , makeDoor
    , open
    , close
    , lock
    , unlock
    , SingState(SOpened, SClosed, SLocked)
    , SomeDoor(SomeDoor)
    ) where

import Data.Kind (Type)
import Data.Text (Text)

data State = Opened | Closed | Locked

data Door (state :: State) = Door {
    name :: Text
} deriving (Show, Eq)

makeDoor :: Text -> Door 'Locked
makeDoor name = Door name

open :: Door 'Closed -> Door 'Opened
open (Door name) = Door name

close :: Door 'Opened -> Door 'Closed
close (Door name) = Door name

lock :: Door 'Closed -> Door 'Locked
lock (Door name) = Door name

unlock :: Door 'Locked -> Door 'Closed
unlock (Door name) = Door name


data SingState :: State -> Type where
    SOpened :: SingState 'Opened
    SClosed :: SingState 'Closed
    SLocked :: SingState 'Locked


data SomeDoor = forall state. SomeDoor (SingState state) (Door state)
