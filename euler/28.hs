value = sum (values tr n) +
        sum (values br n) - 1 +
        sum (values tl n) - 1 +
        sum (values bl n) - 1
    where
      n = (1001 - 1) `div` 2 + 1
      tr n = 8*(n - 1)
      br n = 8*(n - 2) + 2
      tl n = 8*(n - 2) + 6
      bl n = 8*(n - 2) + 4

values _ 1 = [1]
values f n = let p = values f (n - 1)
             in (head p + f n):p

{-
value = sum (map topRight [1..n]) +
        sum (map bottomRight [2..n]) +
        sum (map topLeft [2..n]) +
        sum (map bottomLeft [2..n])
    where
      n = (1001 - 1) `div` 2 + 1

topRight 1 = 1
topRight n = topRight (n - 1) + 8*(n - 1)

bottomRight 1 = 1
bottomRight n = bottomRight (n - 1) + 8*(n - 2) + 2

topLeft 1 = 1
topLeft n = topLeft (n - 1) + 8*(n - 2) + 6

bottomLeft 1 = 1
bottomLeft n = bottomLeft (n - 1) + 8*(n - 2) + 4
-}
