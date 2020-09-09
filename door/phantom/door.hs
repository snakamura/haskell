{-# LANGUAGE DataKinds,
             KindSignatures,
             TypeFamilies
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , name
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , knock
    ) where

import Data.Kind (Constraint)
import Data.Text (Text)

data State = Opened | Closed | Locked

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

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

knock :: Knockable state => Door state -> Door state
knock = id

{-
class Knockable state where
    knock :: Door state -> Door state
    knock = id
instance Knockable 'Closed
instance Knockable 'Locked
-}
