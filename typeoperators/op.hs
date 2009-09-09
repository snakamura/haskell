{-# LANGUAGE TypeOperators #-}

data name ::: value = name := value deriving Show

value :: name ::: value -> value
value (n := v) = v
