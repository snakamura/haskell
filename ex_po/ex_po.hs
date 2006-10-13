data Ex = forall a. Num a => Ex a

data Po = Po (forall a. Num a => a)

{-
addEx :: Ex -> Ex -> Ex
addEx (Ex x) (Ex y) = Ex (x + y)
-}

addPo :: Po -> Po -> Po
addPo (Po x) (Po y) = Po (x + y)

data Num a => Po2 a = Po2 a

addPo2 :: Num a => Po2 a -> Po2 a -> Po2 a
addPo2 (Po2 x) (Po2 y) = Po2 (x + y)

{-
l :: [forall a. Num a => a]
l = []
-}

data X a = X a

data Y = forall a. Y a

data Num a => Z a = Z a

data W = W (forall a. Num a => a)

