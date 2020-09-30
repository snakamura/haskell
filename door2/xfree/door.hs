{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings,
             PatternSynonyms,
             QuasiQuotes,
             StandaloneDeriving,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies
#-}

module Door
    ( Door
    , State(Opened, Closed, Locked, Unlocking)
    , Action
    , SUnlockResult(SUnlocked, SNotUnlocked)
    , makeClosed
    , open
    , close
    , lock
    , tryUnlocking
    , commitUnlocking
    , knock
    , run
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as S
import Control.XFreer
    ( XFree(Bind, Pure)
    , xfree
    )
import Data.Kind (Constraint)
import Data.Singletons (pattern FromSing)
import Data.Singletons.TH (singletons)
import Data.Text (Text)
import qualified Data.Text.IO as T

singletons [d|
    data State = Opened | Closed | Locked | Unlocking
  |]

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

singletons [d|
    data UnlockResult = Unlocked | NotUnlocked

    unlockState Unlocked = Closed
    unlockState NotUnlocked = Locked
  |]

data Action before after a where
    Open :: Action 'Closed 'Opened ()
    Close :: Action 'Opened 'Closed ()
    Lock :: Text -> Action 'Closed 'Locked ()
    TryUnlocking :: Text -> Action 'Locked 'Unlocking UnlockResult
    CommitUnlocking :: SUnlockResult result -> Action 'Unlocking (UnlockState result) ()
    Knock :: Knockable state => Action state state ()


open :: XFree Action Closed Opened ()
open = xfree Open

close :: XFree Action Opened Closed ()
close = xfree Close

lock :: Text -> XFree Action Closed Locked ()
lock = xfree . Lock

tryUnlocking :: Text -> XFree Action Locked Unlocking UnlockResult
tryUnlocking = xfree . TryUnlocking

commitUnlocking :: SUnlockResult result -> XFree Action Unlocking (UnlockState result) ()
commitUnlocking = xfree . CommitUnlocking

knock :: Knockable state => XFree Action state state ()
knock = xfree Knock


data Door (state :: State) = Door {
    name :: Text
} deriving (Show, Eq)

makeClosed :: Text -> Door 'Closed
makeClosed = Door


runAction :: Door before -> Action before after a -> S.StateT (Maybe Text) IO a
runAction door Open = runAction' "open" door
runAction door Close = runAction' "close" door
runAction door (Lock key) = do
    S.put $ Just key
    runAction' "lock" door
runAction door (TryUnlocking key) = do
    currentKey <- S.get
    if maybe False (== key) currentKey then do
        S.put Nothing
        runAction' "unlocking" door
        pure Unlocked
    else do
        runAction' "fail unlocking" door
        pure NotUnlocked
runAction door (CommitUnlocking result) = runAction' "unlock" door
runAction door Knock = runAction' "knock" door

runAction' :: Text -> Door state -> S.StateT (Maybe Text) IO ()
runAction' action door = liftIO $ T.putStrLn $ action <> " " <> name door

run :: Door before -> XFree Action before after () -> IO ()
run door action = S.evalStateT (run' door action) Nothing

run' :: Door before -> XFree Action before after () -> S.StateT (Maybe Text) IO ()
run' door (Bind action f) = runAction door action >>= \a -> run' (Door $ name door) (f a)
run' _ (Pure ()) = pure ()
