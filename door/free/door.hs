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


validateActions :: (S.MonadState Door m, MonadFail m) => Free Action () -> m ()
validateActions (Free (Open next)) = validateActions' [(Closed, Opened)] next
validateActions (Free (Close next)) = validateActions' [(Opened, Closed)] next
validateActions (Free (Lock next)) = validateActions' [(Closed, Locked)] next
validateActions (Free (Unlock next)) = validateActions' [(Locked, Closed)] next
validateActions (Free (Knock next)) = validateActions' [(Closed, Closed), (Locked, Locked)] next
validateActions (Pure r) = pure r

validateActions' :: (S.MonadState Door m, MonadFail m) => [(State, State)] -> Free Action () -> m ()
validateActions' states next = do
    door <- S.get
    case lookup (state door) states of
        Just state -> do
            S.put door { state = state }
            validateActions next
        Nothing -> fail "Invalid state"

validate :: Door -> Free Action () -> Maybe Door
validate door action = S.execStateT (validateActions action) door


runActions :: (S.MonadState Door m, MonadFail m, MonadIO m) => Free Action () -> m ()
runActions (Free (Open next)) = runActions' "open" [(Closed, Opened)] next
runActions (Free (Close next)) = runActions' "close" [(Opened, Closed)] next
runActions (Free (Lock next)) = runActions' "lock" [(Closed, Locked)] next
runActions (Free (Unlock next)) = runActions' "unlock" [(Locked, Closed)] next
runActions (Free (Knock next)) = runActions' "knock" [(Closed, Closed), (Locked, Locked)] next
runActions (Pure r) = pure r

runActions' :: (S.MonadState Door m, MonadFail m, MonadIO m) => Text -> [(State, State)] -> Free Action () -> m ()
runActions' action states next = do
    door <- S.get
    case lookup (state door) states of
        Just state -> do
            S.put door { state = state }
            liftIO $ T.putStrLn $ action <> " " <> name door
            runActions next
        Nothing -> fail "Invalid state"

run :: Door -> Free Action () -> IO ()
run door action = S.evalStateT (runActions action) door
