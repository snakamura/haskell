import Data.Sequence

data Vector a = BoolVector [Bool]
              | IntVector (Seq Int)

add :: a -> Vector a -> Vector a
add v (BoolVector l) = BoolVector $ v:l
add v (IntVector s) = IntVector $ v <| s
