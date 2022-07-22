import Base
import Add
import Stringify
import Array

instance (Stringify a, Stringify b) => Stringify (Cons a b) where
    stringify (Cons a b) = stringify a <> stringify b
instance Stringify Nil where
    stringify Nil = ""

e = Add (Literal 5) (Literal 10) `Cons` Literal 3 `Cons` Nil
n = eval e
s = stringify e
