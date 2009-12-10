import Control.Concurrent

main =
  do chan <- newChan
     forkIO $ sub chan
     process chan
  where
    process chan =
      do d <- readChan chan
         case d of
           Just i -> putStrLn (show i) >> process chan
           Nothing -> return ()

sub chan =
  do mapM_ (writeChan chan . Just) [1..10]
     writeChan chan Nothing
