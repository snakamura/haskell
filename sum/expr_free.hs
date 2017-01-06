{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free (Free(Free), iter, liftF)

data ExprF e = ZeroF
             | SuccF e
             | AddF e e
  deriving (Show, Functor)

type Expr a = Free ExprF a

zeroF :: Expr a
zeroF = liftF ZeroF

succF :: Expr ()
succF = liftF $ SuccF ()

addF :: Expr Bool
addF = liftF $ AddF True False

sumExprF :: Expr a -> Expr a
sumExprF (Free ZeroF) = zeroF
sumExprF e1@(Free (SuccF e2)) = addF >>= \b -> if b then e1 else sumExprF e2
sumExprF e@(Free (AddF _ _)) = sumExprF $ reduce e

reduce :: Expr a -> Expr a
reduce (Free ZeroF) = zeroF
reduce (Free (SuccF e)) = succF >> reduce e
reduce (Free (AddF (Free ZeroF) e)) = reduce e
reduce (Free (AddF (Free (SuccF e1)) e2)) = reduce $ addF >>= \b -> if b then e1 else succF >> e2
reduce (Free (AddF e1@(Free (AddF _ _)) e2)) = reduce $ addF >>= \b -> if b then reduce e1 else e2

valueF :: Num a => ExprF a -> a
valueF ZeroF = 0
valueF (SuccF e) = 1 + e
valueF (AddF e1 e2) = e1 + e2


sum5 :: Num a => a
sum5 = iter valueF $ sumExprF $ do
    succF
    succF
    succF
    succF
    succF
    zeroF

sum5' :: Num a => a
sum5' = iter valueF $ sumExprF $ do
    b <- addF
    if b then do
        succF
        succF
        succF
        zeroF
    else do
        succF
        succF
        zeroF
