{-# LANGUAGE FlexibleContexts,
             GADTs,
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
import Data.Functor.Coyoneda
    ( Coyoneda(Coyoneda)
    , liftCoyoneda
    )
import qualified Control.Monad.State as S
import Data.Text (Text)
import qualified Data.Text.IO as T

data Action a where
    Open :: Action ()
    Close :: Action ()
    Lock :: Action ()
    Unlock :: Action ()
    Knock :: Action ()

open, close, lock, unlock, knock :: Free (Coyoneda Action) ()
open = liftF $ liftCoyoneda Open
close = liftF $ liftCoyoneda Close
lock = liftF $ liftCoyoneda Lock
unlock = liftF $ liftCoyoneda Unlock
knock = liftF $ liftCoyoneda Knock


data State = Opened | Closed | Locked deriving (Show, Eq, Ord, Enum, Bounded)

data Door = Door {
    name :: Text,
    state :: State
} deriving (Show, Eq)

makeLocked :: Text -> Door
makeLocked name = Door name Locked


runAction :: (S.MonadState Door m, MonadFail m, MonadIO m) => Action a -> m a
runAction Open = runAction' "open" [(Closed, Opened)]
runAction Close = runAction' "close" [(Opened, Closed)]
runAction Lock = runAction' "lock" [(Closed, Locked)]
runAction Unlock = runAction' "unlock" [(Locked, Closed)]
runAction Knock = runAction' "knock" [(Closed, Closed), (Locked, Locked)]

runAction' :: (S.MonadState Door m, MonadFail m, MonadIO m) => Text -> [(State, State)] -> m ()
runAction' action states = do
    door <- S.get
    case lookup (state door) states of
        Just state -> do
            S.put door { state = state }
            liftIO $ T.putStrLn $ action <> " " <> name door
        Nothing -> fail "Invalid state"


runActions :: (S.MonadState Door m, MonadFail m, MonadIO m) => Free (Coyoneda Action) a -> m a
runActions (Free (Coyoneda next action)) = runAction action >>= runActions . next
runActions (Pure r) = pure r

run :: Door -> Free (Coyoneda Action) () -> IO ()
run door action = S.evalStateT (runActions action) door
