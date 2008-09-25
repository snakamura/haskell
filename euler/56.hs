import Data.Char

value = maximum [ digitalSum (a^b) | a <- [1..99], b <- [1..99] ]

digitalSum = sum . map digitToInt . show
