import Data.IORef
import Control.Monad.Fix

data List = Cons {car::Int, cdr::(IORef List)} | Nil

main = mdo
  p <- newIORef (Cons 1 p)
  ones <- readIORef p
  print (car ones)

test = do
    p <- mfix (\q -> newIORef (Cons 1 q))
    ones <- readIORef p
    print (car ones)
