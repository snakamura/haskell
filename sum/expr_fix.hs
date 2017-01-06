{-# LANGUAGE DeriveFunctor #-}

import Data.Fix (Fix(Fix), (~>), ana, cata)

data ExprF e = ZeroF
             | SuccF e
             | AddF e e
  deriving (Show, Functor)

zeroF :: Fix ExprF
zeroF = Fix ZeroF

succF :: Fix ExprF -> Fix ExprF
succF = Fix . SuccF

addF :: Fix ExprF -> Fix ExprF -> Fix ExprF
addF = (Fix .) . AddF

sumExprF :: Fix ExprF -> Fix ExprF
sumExprF (Fix ZeroF) = zeroF
sumExprF (Fix (SuccF e)) = addF (succF e) (sumExprF e)

valueF :: Num a => ExprF a -> a
valueF ZeroF = 0
valueF (SuccF e) = 1 + e
valueF (AddF e1 e2) = e1 + e2


sum5 :: Num a => a
sum5 = cata valueF (sumExprF (succF (succF (succF (succF (succF zeroF))))))


unValueF :: (Num a, Eq a) => a -> ExprF a
unValueF 0 = ZeroF
unValueF n = SuccF (n - 1)

sum5' :: Num a => a
sum5' = cata valueF (sumExprF (ana unValueF 5))

_5 :: Double
_5 = (unValueF ~> valueF) (5 :: Int)
