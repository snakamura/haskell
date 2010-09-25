{-# LANGUAGE Generics, TypeOperators #-}

import Data.Generics

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
    incrementSalary {| Unit |} r Unit = Unit
    incrementSalary {| a :+: b |} r (Inl x) = Inl (incrementSalary r x)
    incrementSalary {| a :+: b |} r (Inr x) = Inr (incrementSalary r x)
    incrementSalary {| a :*: b |} r (x :*: y) = (incrementSalary r x) :*: (incrementSalary r y)

instance IncrementSalary Company

instance IncrementSalary Dept

instance IncrementSalary Unt

instance IncrementSalary Employee

instance IncrementSalary Person

instance IncrementSalary Salary where
    incrementSalary r (S s) = S (s * r)

instance IncrementSalary a => IncrementSalary [a] where

instance IncrementSalary Char where
    incrementSalary _ a = a


company = C [D "Dept 1" (E (P "Dept 1 Manager" "") (S 100))
                   [PU (E (P "1" "") (S 10)),
                    DU (D "Dept 1-1" (E (P "Dept 1-1 Manager" "") (S 50)) [])]]
