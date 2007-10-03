permutation l  = p $ length l
 where
     p 0 = [[]]
     p n = [ x:y | x <- l, y <- filter (notElem x) $ p $ n - 1 ]
