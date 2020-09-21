{-# LANGUAGE DeriveFunctor,
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

import Control.Monad.Indexed.Free
    ( IxFree(Free, Pure)
    , iliftFree
    )
import Data.Functor.Indexed
    ( IxFunctor
    , imap
    )
import Data.Kind (Constraint)
import Data.Text (Text)
import qualified Data.Text.IO as T

data Opened
data Closed
data Locked

type family Knockable state :: Constraint where
    Knockable Closed = ()
    Knockable Locked = ()

data Action before after r where
    Open :: r -> Action Closed Opened r
    Close :: r -> Action Opened Closed r
    Lock :: r -> Action Closed Locked r
    Unlock :: r -> Action Locked Closed r
    Knock :: Knockable state => r -> Action state state r

deriving instance Functor (Action b a)

instance IxFunctor Action where
    imap = fmap

open :: IxFree Action Closed Opened ()
open = iliftFree $ Open ()

close :: IxFree Action Opened Closed ()
close = iliftFree $ Close ()

lock :: IxFree Action Closed Locked ()
lock = iliftFree $ Lock ()

unlock :: IxFree Action Locked Closed ()
unlock = iliftFree $ Unlock ()

knock :: Knockable state => IxFree Action state state ()
knock = iliftFree $ Knock ()


data Door state = Door {
    name :: Text
} deriving (Show, Eq)

makeLocked :: Text -> Door Locked
makeLocked = Door


run :: Door before -> IxFree Action before after a -> IO a
run door (Free (Open next)) = run' "open" door next
run door (Free (Close next)) = run' "close" door next
run door (Free (Lock next)) = run' "lock" door next
run door (Free (Unlock next)) = run' "unlock" door next
run door (Free (Knock next)) = run' "knock" door next
run _ (Pure r) = pure r

run' :: Text -> Door before -> IxFree Action after state a -> IO a
run' action door next = do
    T.putStrLn $ action <> " " <> name door
    run (Door $ name door) next
