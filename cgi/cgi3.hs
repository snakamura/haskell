{-# OPTIONS_GHC -fallow-overlapping-instances #-}

import Data.Maybe
import Network.NewCGI
import Text.XHtml

main :: IO ()
main = runCGI (handleErrors cgiMain)


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-Type" "text/html; charset=utf-8"
             name <- getInput "name"
             output $ renderHtml $ page $ fromMaybe "Guest" name
 where
     page name = header << (meta ! [httpequiv "content-type", content "text/html; charset=utf-8"] +++
                           thetitle << "Test") +++
                 body << p << ("Hello, " ++ name ++ "!")
