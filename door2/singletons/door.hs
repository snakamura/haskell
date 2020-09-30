{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             QuasiQuotes,
             StandaloneDeriving,
             TemplateHaskell,
             TypeFamilies
#-}

module Door
    ( Door(OpenedDoor, ClosedDoor, LockedDoor)
    , State(Opened, Closed, Locked)
    , SState(SOpened, SClosed, SLocked)
    , MaybeUnlocked(Unlocked, NotUnlocked)
    , MaybeUnlockedDoor
    , UnlockResult
    , SUnlockResult(SSuccess, SFail)
    , name
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , tryUnlock
    , commitUnlock
    , knock
    ) where

import Data.Kind (Constraint)
import Data.Singletons (SingI)
import Data.Singletons.Sigma (Sigma((:&:)))
import Data.Singletons.TH (singletons)
import Data.Text (Text)

singletons [d|
    data State = Opened | Closed | Locked
  |]

data Door (state :: State) where
    OpenedDoor :: Text -> Door 'Opened
    ClosedDoor :: Text -> Door 'Closed
    LockedDoor :: Text -> Text -> Door 'Locked

deriving instance Show (Door state)
deriving instance Eq (Door state)

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


data MaybeUnlocked state where
    Unlocked:: Door 'Closed -> MaybeUnlocked 'Closed
    NotUnlocked :: Door 'Locked -> MaybeUnlocked 'Locked

singletons [d|
    type MaybeUnlockedDoor s = MaybeUnlocked s
  |]

unlock :: Text -> Door 'Locked -> Sigma State MaybeUnlockedDoorSym0
unlock key door@(LockedDoor name lockedKey)
    | lockedKey == key = SClosed :&: Unlocked (ClosedDoor name)
    | otherwise = SLocked :&: NotUnlocked door


-- These tryUnlock and commitUnlock withUnlockResult don't make much sense
-- because you can always pass SSuccess to commitUnlock.
singletons [d|
    data UnlockResult = Success | Fail
  |]

type family UnlockedState result where
    UnlockedState Success = 'Closed
    UnlockedState Fail = 'Locked

tryUnlock :: Text -> Door 'Locked -> UnlockResult
tryUnlock key door@(LockedDoor name lockedKey)
    | lockedKey == key = Success
    | otherwise = Fail

commitUnlock :: SUnlockResult result -> Door 'Locked -> Door (UnlockedState result)
commitUnlock result door@(LockedDoor _ name) = case result of
    SSuccess -> ClosedDoor name
    SFail -> door


type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

knock :: Knockable state => Door state -> Door state
knock = id
