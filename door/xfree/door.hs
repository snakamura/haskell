{-# LANGUAGE DataKinds,
             GADTs,
             OverloadedStrings,
             StandaloneDeriving,
             TypeFamilies
#-}

module Door
    ( Door
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , knock
    , run
    ) where

import Control.XFreer
    ( XFree(Bind, Pure)
    , xfree
    )
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text.IO as T

data State = Opened | Closed | Locked

type family Knockable (state :: State) :: Constraint where
    Knockable 'Closed = ()
    Knockable 'Locked = ()

data Action before after a where
    Open :: Action 'Closed 'Opened ()
    Close :: Action 'Opened 'Closed ()
    Lock :: Action 'Closed 'Locked ()
    Unlock :: Action 'Locked 'Closed ()
    Knock :: Knockable state => Action state state ()


open :: XFree Action Closed Opened ()
open = xfree $ Open

close :: XFree Action Opened Closed ()
close = xfree $ Close

lock :: XFree Action Closed Locked ()
lock = xfree $ Lock

unlock :: XFree Action Locked Closed ()
unlock = xfree $ Unlock

knock :: Knockable state => XFree Action state state ()
knock = xfree $ Knock


data Door (state :: State) = Door {
    name :: Text
} deriving (Show, Eq)

makeLocked :: Text -> Door 'Locked
makeLocked = Door


runAction :: Door before -> Action before after a -> IO a
runAction door Open = runAction' "open" door
runAction door Close = runAction' "close" door
runAction door Lock = runAction' "lock" door
runAction door Unlock = runAction' "unlock" door
runAction door Knock = runAction' "knock" door

runAction' :: Text -> Door state -> IO ()
runAction' action door = T.putStrLn $ action <> " " <> name door

run :: Door before -> XFree Action before after () -> IO ()
run door (Bind action f) = runAction door action >>= \a -> run (Door $ name door) (f a)
run _ (Pure ()) = pure ()
