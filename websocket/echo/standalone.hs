module Main (main) where

import App (app)
import qualified Network.WebSockets as WS


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 app
