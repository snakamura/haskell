{-# OPTIONS_GHC -fallow-overlapping-instances #-}

import Network.NewCGI
import Text.XHtml

main :: IO ()
main = runCGI (handleErrors cgiMain)


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-Type" "text/html; charset=utf-8"
             output $ renderHtml page
 where
     page = header << (meta ! [httpequiv "content-type", content "text/html; charset=utf-8"] +++
                      thetitle << "Test") +++
            body << p << "Hello, world!"
