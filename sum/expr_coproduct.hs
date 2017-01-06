{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}

import Data.Fix (Fix(Fix), cata)
import Data.Functor.Classes (Show1, showsPrec1)
import Data.Functor.Sum (Sum(InL, InR))

data ZeroF e = ZeroF deriving (Show, Functor)
data SuccF e = SuccF e deriving (Show, Functor)
data AddF e = AddF e e deriving (Show, Functor)

instance Show1 ZeroF where
    showsPrec1 = showsPrec
instance Show1 SuccF where
    showsPrec1 = showsPrec
instance Show1 AddF where
    showsPrec1 = showsPrec

class (Functor f, Functor g) => Super f g where
    inj :: f a -> g a
    app :: (f a -> a) -> (g a -> a)
instance {-# OVERLAPPABLE #-} Functor f => Super f f where
    inj = id
    app = id
instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => Super f (Sum f g) where
    inj = InL
    app f (InL l) = f l
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, Super f g) => Super f (Sum h g) where
    inj = InR . inj
    app f (InR r) = app f r

type ExprF = Sum ZeroF (Sum SuccF AddF)

zeroF :: Fix ExprF
--zeroF = Fix (InL ZeroF)
zeroF = Fix (inj ZeroF)

succF :: Fix ExprF -> Fix ExprF
--succF = Fix . InR . InL . SuccF
succF = Fix . inj . SuccF

addF :: Fix ExprF -> Fix ExprF -> Fix ExprF
--addF = ((Fix . InR . InR) .) . AddF
addF = ((Fix . inj) .) . AddF

sumExprF :: Fix ExprF -> Fix ExprF
sumExprF (Fix (InL ZeroF)) = zeroF
sumExprF (Fix (InR (InL (SuccF e)))) = addF (succF e) (sumExprF e)

class ValueF f where
    valueF :: Num a => f a -> a
instance ValueF ZeroF where
    valueF _ = 0
instance ValueF SuccF where
    valueF (SuccF e) = 1 + e
instance ValueF AddF where
    valueF (AddF e1 e2) = e1 + e2
instance (ValueF f, ValueF g) => ValueF (Sum f g) where
    valueF (InL f) = valueF f
    valueF (InR g) = valueF g
{-
valueF :: Num a => ExprF a -> a
valueF (InL ZeroF) = 0
valueF (InR (InL (SuccF e))) = 1 + e
valueF (InR (InR (AddF e1 e2))) = e1 + e2
-}


sum5 :: Num a => a
--sum5 = cata (app valueF) (sumExprF (succF (succF (succF (succF (succF zeroF))))))
sum5 = cata valueF (sumExprF (succF (succF (succF (succF (succF zeroF))))))
