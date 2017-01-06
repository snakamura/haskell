{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free (Free(Free, Pure), iter, liftF)

data ExprF e = SuccF e
             | AddF e e
  deriving (Show, Functor)

type Expr a = Free ExprF a

zeroF :: Num a => Expr a
zeroF = Pure 0

succF :: Expr ()
succF = liftF $ SuccF ()

addF :: Expr Bool
addF = liftF $ AddF True False

sumExprF :: Expr a -> Expr a
sumExprF e@(Pure _) = e
sumExprF e1@(Free (SuccF e2)) = do
    b <- addF
    if b then
        e1
    else
        sumExprF e2
sumExprF e@(Free (AddF _ _)) = sumExprF $ reduce e

reduce :: Expr a -> Expr a
reduce e@(Pure _) = e
reduce (Free (SuccF e)) = succF >> reduce e
reduce (Free (AddF (Pure _) e)) = reduce e
reduce (Free (AddF (Free (SuccF e1)) e2)) = reduce $ do
    b <- addF
    if b then
        e1
    else do
        succF
        e2
reduce (Free (AddF e1@(Free (AddF _ _)) e2)) = reduce $ do
    b <- addF
    if b then
        reduce e1
    else
        e2

valueF :: Num a => ExprF a -> a
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
