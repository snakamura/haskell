import Control.Concurrent.STM

main =
  do n <- newEmptyTMVarIO
     atomically $ putTMVar n 1
