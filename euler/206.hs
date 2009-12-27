import Data.List.Split

main = print value

value = head [ x | x <- [s, s + 10..e], match (x * x)]
  where
    s = truncate $ sqrt 1020304050607080900
    e = truncate $ sqrt 1929394959697989990

match x = map head (splitEvery 2 (show x)) == "1234567890"
