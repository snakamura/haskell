import Control.Concurrent

main = 
  do end <- newEmptyMVar
     forkIO $ sub end
     putStrLn "main"
     takeMVar end
     return ()

sub end = 
  do threadDelay $ 1*1000*1000
     putStrLn "sub"
     putMVar end ()
