{-# OPTIONS_GHC -fallow-overlapping-instances #-}

import Network.NewCGI

main :: IO ()
main = runCGI (handleErrors cgiMain)


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-Type" "text/plain; charset=us-ascii"
             output "Hello, world!"
