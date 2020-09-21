{-# LANGUAGE DeriveFunctor,
             FlexibleContexts,
             OverloadedStrings
#-}

module Door
    ( Door
    , makeLocked
    , open
    , close
    , lock
    , unlock
    , knock
    , validate
    , run
    ) where

import Control.Monad.Free
    ( Free(Free, Pure)
    , iterM
    , liftF
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import qualified Control.Monad.State as S
import Data.Text (Text)
import qualified Data.Text.IO as T

data Action r = Open r
              | Close r
              | Lock r
              | Unlock r
              | Knock r
  deriving Functor

open, close, lock, unlock, knock :: Free Action ()
open = liftF $ Open ()
close = liftF $ Close ()
lock = liftF $ Lock ()
unlock = liftF $ Unlock ()
knock = liftF $ Knock ()


data State = Opened | Closed | Locked deriving (Show, Eq, Ord, Enum, Bounded)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)

makeLocked :: Text -> Door
makeLocked name = Door name Locked

validateAction :: (S.MonadState Door m, MonadFail m) => Action (m a) -> m a
validateAction (Open next) = validateAction' [(Closed, Opened)] >> next
validateAction (Close next) = validateAction' [(Opened, Closed)] >> next
validateAction (Lock next) = validateAction' [(Closed, Locked)] >> next
validateAction (Unlock next) = validateAction' [(Locked, Closed)] >> next
validateAction (Knock next) = validateAction' [(Closed, Closed), (Locked, Locked)] >> next

validateAction' :: (S.MonadState Door m, MonadFail m) => [(State, State)] -> m ()
validateAction' states = do
    door <- S.get
    case lookup (state door) states of
        Just state -> S.put door { state = state }
        Nothing -> fail "Invalid state"

validateActions :: (S.MonadState Door m, MonadFail m) => Free Action a -> m a
validateActions = iterM validateAction

validate :: Door -> Free Action a -> Maybe Door
validate door action = S.execStateT (validateActions action) door


runAction :: (S.MonadState Door m, MonadFail m, MonadIO m) => Action (m a) -> m a
runAction (Open next) = runAction' "open" [(Closed, Opened)] >> next
runAction (Close next) = runAction' "close" [(Opened, Closed)] >> next
runAction (Lock next) = runAction' "lock" [(Closed, Locked)] >> next
runAction (Unlock next) = runAction' "unlock" [(Locked, Closed)] >> next
runAction (Knock next) = runAction' "knock" [(Closed, Closed), (Locked, Locked)] >> next

runAction' :: (S.MonadState Door m, MonadFail m, MonadIO m) => Text -> [(State, State)] -> m ()
runAction' action states = do
    door <- S.get
    case lookup (state door) states of
        Just state -> do
            S.put door { state = state }
            liftIO $ T.putStrLn $ action <> " " <> name door
        Nothing -> fail "Invalid state"

runActions :: (S.MonadState Door m, MonadFail m, MonadIO m) => Free Action a -> m a
runActions = iterM runAction

run :: Door -> Free Action a -> IO a
run door action = S.evalStateT (runActions action) door
