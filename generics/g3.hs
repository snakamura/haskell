{-# LANGUAGE TypeOperators, TypeFamilies #-}

data U = U
data a :*: b = a :*: b
data a :+: b = L a
             | R b
data V a = V a

class R a where
    type S a
    from :: a -> S a
    to :: S a -> a

instance R U where
    type S U = U
    from = id
    to = id

instance (R a, R b) => R (a :*: b) where
    type S (a :*: b) = a :*: b
    from = id
    to = id

instance (R a, R b) => R (a :+: b) where
    type S (a :+: b) = a :+: b
    from = id
    to = id

instance R (V a) where
    type S (V a) = V a
    from = id
    to = id

instance R Int where
    type S Int = Int
    from = id
    to = id

instance R Char where
    type S Char = Char
    from = id
    to = id

instance R a => R [a] where
    type S [a] = U :+: (a :*: [a])
    from [] = L U
    from (x:xs) = R (x :*: xs)
    to (L U) = []
    to (R (x :*: xs)) = x:xs


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

instance IncrementSalary a => IncrementSalary (V a) where
    incrementSalary r (V x) = V (incrementSalary r x)

instance R Company where
    type S Company = V [Dept]
    from (C depts) = V depts
    to (V depts) = C depts

instance R Dept where
    type S Dept = Name :*: Manager :*: [Unt]
    from (D n m u) = n :*: m :*: u
    to (n :*: m :*: u) = D n m u

instance R Unt where
    type S Unt = Employee :+: Dept
    from (PU e) = L e
    from (DU d) = R d
    to (L e) = PU e
    to (R d) = DU d

instance R Employee where
    type S Employee = Person :*: Salary
    from (E p s) = p :*: s
    to (p :*: s) = E p s

instance R Person where
    type S Person = Name :*: Address
    from (P n a) = n :*: a
    to (n :*: a) = P n a

instance R Salary where
    type S Salary = V Float
    from (S s) = V s
    to (V s) = S s


instance IncrementSalary Company where
    incrementSalary r c = to (incrementSalary r (from c))

instance IncrementSalary Dept where
    incrementSalary r c = to (incrementSalary r (from c))

instance IncrementSalary Unt where
    incrementSalary r c = to (incrementSalary r (from c))

instance IncrementSalary Employee where
    incrementSalary r c = to (incrementSalary r (from c))

instance IncrementSalary Person where
    incrementSalary r c = to (incrementSalary r (from c))

instance IncrementSalary Salary where
    incrementSalary r (S s) = S (s * r)

instance IncrementSalary Char where
    incrementSalary _ c = c

instance (R a, IncrementSalary a) => IncrementSalary [a] where
    incrementSalary r c = to (incrementSalary r (from c))

company = C [D "Dept 1" (E (P "Dept 1 Manager" "") (S 100))
                   [PU (E (P "1" "") (S 10)),
                    DU (D "Dept 1-1" (E (P "Dept 1-1 Manager" "") (S 50)) [])]]
