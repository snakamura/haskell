module Main (main) where

import App (app)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (intercept)


main :: IO ()
main = Warp.runSettings settings undefined
    where
      settings = Warp.defaultSettings {
                   Warp.settingsPort = 8080,
                   Warp.settingsIntercept = intercept app
                 }
