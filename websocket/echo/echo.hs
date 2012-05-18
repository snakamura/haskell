module Main (main) where

import Control.Exception (fromException)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import qualified Network.WebSockets as WS
import System.IO (hPutStrLn,
                  stderr)


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 app


app :: WS.Request ->
       WS.WebSockets WS.Hybi00 ()
app request = do
  WS.acceptRequest request
  version <- WS.getVersion
  liftIO $ hPutStrLn stderr $ "Version: " ++ version
  WS.catchWsError (forever echo) handler
    where
      handler e = case fromException e of
                    Just WS.ConnectionClosed -> return ()
                    _ -> WS.throwWsError e


echo :: WS.TextProtocol p =>
        WS.WebSockets p ()
echo = do
  message <- WS.receiveData
  liftIO $ hPutStrLn stderr $ "Message: " ++ T.unpack message
  WS.sendTextData message
  return ()
