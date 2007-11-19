{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix
import Data.IORef

data List = Cons {car::Int, cdr::(IORef List)} | Nil

main = undefined


test0 = mdo
  p <- newIORef (Cons 1 p)
  ones <- readIORef p
  print $ car ones

test1 = do
--    p <- mfix (\q -> newIORef (Cons 1 q))
    (_, p) <- mfix (\ ~(p, _) -> do
        p <- newIORef (Cons 1 p);
        return (p, p))
    ones <- readIORef p
    print $ car ones


test2 = mdo
    p <- newIORef (Cons 1 q)
    q <- newIORef (Cons 2 p)
    x <- readIORef p
    y <- readIORef q
    printCarCadr x -- (1,2)
    printCarCadr y -- (2,1)


test3 = do
    (p, q) <- mfix (\ ~(p, q) -> do
        p <- newIORef (Cons 1 q)
        q <- newIORef (Cons 2 p)
        return (p, q))
    x <- readIORef p
    y <- readIORef q
    printCarCadr x -- (1,2)
    printCarCadr y -- (2,1)

printCarCadr x = do
  y <- readIORef (cdr x)
  print (car x, car y)
