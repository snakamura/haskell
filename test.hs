class Test a b | a -> b where
    something :: a -> b
    anything :: a

data X = X

instance Test X Int where
    something x = 5
    anything = X

f :: Test a b => a
f = anything
