import Data.Char

value = sum $ filter (\n -> n == f n) [3..9999999]

f = sum . map (fac . digitToInt) . show

fac 0 = 1
fac 1 = 1
fac 2 = 2
fac 3 = 6
fac 4 = 24
fac 5 = 120
fac 6 = 720
fac 7 = 5040
fac 8 = 40320
fac 9 = 362880
fac n = n * fac (n - 1)
