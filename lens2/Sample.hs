module Sample where

data Person = Person
  { _name :: Name,
    _age :: Int
  }
  deriving (Show)

data Name = Name
  { _first :: String,
    _last :: String
  }
  deriving (Show)

scottTiger :: Person
scottTiger =
  Person
    { _name =
        Name
          { _first = "Scott",
            _last = "Tiger"
          },
      _age = 45
    }
