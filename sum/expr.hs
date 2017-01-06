data Expr = Zero
          | Succ Expr
          | Add Expr Expr
  deriving Show

sumExpr :: Expr -> Expr
sumExpr Zero = Zero
sumExpr e1@(Succ e2) = Add e1 (sumExpr e2)
sumExpr e@(Add _ _) = sumExpr $ reduce e

reduce :: Expr -> Expr
reduce Zero = Zero
reduce (Succ e) = Succ (reduce e)
reduce (Add Zero e) = reduce e
reduce (Add (Succ e1) e2) = reduce (Add e1 (Succ e2))
reduce (Add e1@(Add _ _) e2) = reduce (Add (reduce e1) e2)

value :: Num a => Expr -> a
value Zero = 0
value (Succ e) = 1 + value e
value (Add e1 e2) = value e1 + value e2


sum5 :: Num a => a
sum5 = value (sumExpr (Succ (Succ (Succ (Succ (Succ Zero))))))
