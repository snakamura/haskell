{-# LANGUAGE OverloadedStrings,
             RebindableSyntax
#-}

import Control.Monad.Indexed
    ( IxMonad
    , (>>>=)
    )
import Data.String (fromString)
import Door
import Prelude hiding ((>>=), (>>))

runValid :: IO ()
runValid = run (makeLocked "valid") $
    unlock >>>= \_ ->
    knock >>>= \_ ->
    open >>>= \_ ->
    close

{-
runInvalid :: IO ()
runInvalid = run (makeLocked "invalid") $
    knock >>>= \_ ->
    open
-}

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (. const ) . (>>>=)

runValidDo :: IO ()
runValidDo = run (makeLocked "valid") $ do
    unlock
    knock
    open
    close
