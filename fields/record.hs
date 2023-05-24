{-# LANGUAGE RecordWildCards #-}

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

f1, f2, f3, f4 :: String
Person {name = Name {first = f1}} = person
f2 = first $ name person
f3 = (\Person {name = Name {first}} -> first) person
f4 = (\Person {name = Name {..}, ..} -> first) person

a1, a2, a3, a4 :: Int
Person {age = a1} = person
a2 = age person
a3 = (\Person {age} -> age) person
a4 = (\Person {..} -> age) person

p1, p2 :: Person
p1 =
  person
    { name =
        (name person)
          { first = "Micheal"
          }
    }
p2 = person {age = 11}
