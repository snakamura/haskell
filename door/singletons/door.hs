{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , SState(SOpened, SClosed, SLocked)
    , name
    , makeLocked
    , open
    , close
    , lock
    , unlock
    ) where

import Data.Singletons.TH (singletons)
import Data.Text (Text)

singletons [d|
    data State = Opened | Closed | Locked
  |]

data Door (state :: State) = Door {
    name :: Text
} deriving (Show, Eq)

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
