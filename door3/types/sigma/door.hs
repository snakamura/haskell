{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             ScopedTypeVariables,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             UndecidableInstances
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked)
    , SState(SOpened, SClosed, SLocked)
    , SomeDoor(SomeDoor)
    , makeLocked
    , name
    , open
    , close
    , lock
    , unlock
    , knock
) where

import Data.Kind (Constraint, Type)
import Data.Singletons.TH
import Data.Text (Text)
import Sigma (OneOfSym1, SigmaP((:&?:)))

singletons [d|
    data State = Opened | Closed | Locked deriving (Show, Eq)
  |]

data Door :: State -> Type where
    OpenedDoor :: Text -> Door 'Opened
    ClosedDoor :: Text -> Door 'Closed
    LockedDoor :: Text -> Text -> Door 'Locked
deriving instance Show (Door state)

data SomeDoor = forall state. SingI state => SomeDoor (Door state)

name :: Door state -> Text
name (OpenedDoor name) = name
name (ClosedDoor name) = name
name (LockedDoor name _) = name

makeLocked :: Text -> Text -> Door 'Locked
makeLocked name key = LockedDoor name key

open :: Door 'Closed -> Door 'Opened
open (ClosedDoor name) = OpenedDoor name

close :: Door 'Opened -> Door 'Closed
close (OpenedDoor name) = ClosedDoor name

lock :: Text -> Door 'Closed -> Door 'Locked
lock key (ClosedDoor name) = LockedDoor name key

unlock :: Text -> Door 'Locked -> SigmaP State (OneOfSym1 '[ 'Closed, 'Locked ]) (TyCon Door)
unlock key lockedDoor@(LockedDoor name lockedKey)
    | key == lockedKey = SClosed :&?: ClosedDoor name
    | otherwise = SLocked :&?: lockedDoor

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

knock :: Knockable state => Door state -> Door state
knock = id
