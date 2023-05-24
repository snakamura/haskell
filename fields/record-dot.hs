{-# LANGUAGE OverloadedRecordDot #-}

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show)

person :: Person
person = Person (Name "Tiger" "Scott") 10

f :: String
f = person.name.first

a :: Int
a = person.age

-- We need to implement setField by ourselves and enable OverloadedRecordUpdate to use these.
{-
p1, p2 :: Person
p1 = person {name.first = "Micheal"}
p2 = person {age = 11}
-}
