{-# LANGUAGE TypeOperators, TypeFamilies, TemplateHaskell, EmptyDataDecls, FlexibleContexts #-}

import Control.Arrow
import Generics.Instant as I
import Generics.Instant.TH

data Company = C [Dept] deriving Show
data Dept = D Name Manager [Unt] deriving Show
data Unt = PU Employee | DU Dept deriving Show
data Employee = E Person Salary deriving Show
data Person = P Name Address deriving Show
data Salary = S Float deriving Show
type Manager = Employee
type Name = String
type Address = String

class IncrementSalary a where
    incrementSalary :: Float -> a -> a

instance IncrementSalary U where
    incrementSalary _ u = u

instance (IncrementSalary a, IncrementSalary b) => IncrementSalary (a :*: b) where
    incrementSalary r (x :*: y) = incrementSalary r x :*: incrementSalary r y

instance (IncrementSalary a, IncrementSalary b) => IncrementSalary (a :+: b) where
    incrementSalary r (L x) = L (incrementSalary r x)
    incrementSalary r (R x) = R (incrementSalary r x)

instance IncrementSalary a => IncrementSalary (Var a) where
    incrementSalary r (Var x) = Var (incrementSalary r x)

instance IncrementSalary a => IncrementSalary (Rec a) where
    incrementSalary r (Rec x) = Rec (incrementSalary r x)

instance IncrementSalary a => IncrementSalary (C c a) where
    incrementSalary r (I.C x) = I.C (incrementSalary r x)

deriveAll ''Company
deriveAll ''Dept
deriveAll ''Unt
deriveAll ''Employee
deriveAll ''Person
deriveAll ''Salary

dft_incrementSalary :: (Representable a, IncrementSalary (Rep a)) => Float -> a -> a
dft_incrementSalary r c = to (incrementSalary r (from c))

simplInstance ''IncrementSalary ''Company 'incrementSalary 'dft_incrementSalary
simplInstance ''IncrementSalary ''Dept 'incrementSalary 'dft_incrementSalary
simplInstance ''IncrementSalary ''Unt 'incrementSalary 'dft_incrementSalary
simplInstance ''IncrementSalary ''Employee 'incrementSalary 'dft_incrementSalary
simplInstance ''IncrementSalary ''Person 'incrementSalary 'dft_incrementSalary

instance IncrementSalary a => IncrementSalary [a] where
    incrementSalary = dft_incrementSalary

instance IncrementSalary Char where
    incrementSalary _ c = c

instance IncrementSalary Float where
    incrementSalary _ f = f

instance IncrementSalary Salary where
    incrementSalary r (S s) = S (s * r)

company = Main.C [D "Dept 1" (E (P "Dept 1 Manager" "") (S 100))
                   [PU (E (P "1" "") (S 10)),
                    DU (D "Dept 1-1" (E (P "Dept 1-1 Manager" "") (S 50)) [])]]
