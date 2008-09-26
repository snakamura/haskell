import Data.List

value :: Int
value = maximum [ read s | n <- [1..99999],
                           l <- drop 2 $ inits [1,2,3,4,5,6,7,8,9],
                           let s = v n l,
                           sort s == "123456789" ]

v :: Int -> [Int] -> String
v n l = concat $ map (show . (* n)) l
