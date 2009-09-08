import Data.Sequence as Seq

class Container c where
    empty :: c a
    add :: a -> c a -> c a

instance Container [] where
    empty = []
    add v l = v:l

instance Container Seq where
    empty = Seq.empty
    add v s = v <| s
