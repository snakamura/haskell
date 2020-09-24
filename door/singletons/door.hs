{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies
#-}

module Door
    ( Door
    , SomeDoor(SomeDoor)
    , State(Opened, Closed, Locked)
    , SState(SOpened, SClosed, SLocked)
    , name
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , knock
    ) where

import Data.Kind (Constraint)
import Data.Singletons (SingI)
import Data.Singletons.TH (singletons)
import Data.Text (Text)

singletons [d|
    data State = Opened | Closed | Locked
  |]

data Door (state :: State) = Door {
    name :: Text
} deriving (Show, Eq)

data SomeDoor = forall state. SingI state => SomeDoor (Door state)

makeLocked :: Text -> Door 'Locked
makeLocked name = Door name

open :: Door 'Closed -> Door 'Opened
open (Door name) = Door name

close :: Door 'Opened -> Door 'Closed
close (Door name) = Door name

lock :: Door 'Closed -> Door 'Locked
lock (Door name) = Door name

unlock :: Door 'Locked -> Door 'Closed
unlock (Door name) = Door name

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

knock :: Knockable state => Door state -> Door state
knock = id
