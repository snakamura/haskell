{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances  #-}

class Show a => MyShow a where
    toString :: a -> String
    toString = show

instance Show a => MyShow a

instance MyShow String where
    toString = id
