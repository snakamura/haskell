value = sum $ map count [1..1000]

count 1 = 3 -- one
count 2 = 3 -- two
count 3 = 5 -- three
count 4 = 4 -- four
count 5 = 4 -- five
count 6 = 3 -- six
count 7 = 5 -- seven
count 8 = 5 -- eight
count 9 = 4 -- nine
count 10 = 3 -- ten
count 11 = 6 -- eleven
count 12 = 6 -- twelve
count 13 = 8 -- thriteen
count 14 = 8 -- fourteen
count 15 = 7 -- fifteen
count 16 = 7 -- sixteen
count 17 = 9 -- seventeen
count 18 = 8 -- eighteen
count 19 = 8 -- nineteen
count 20 = 6 -- twenty
count 30 = 6 -- thrity
count 40 = 5 -- forty
count 50 = 5 -- fifty
count 60 = 5 -- sixty
count 70 = 7 -- seventy
count 80 = 6 -- eighty
count 90 = 6 -- ninety
count 1000 = 11 -- one thousand
count n | n < 100 = let (m, d) = n `divMod` 10
                    in count (m*10) + count d
        | n < 1000 = let (m, d) = n `divMod` 100
                     in if d == 0 then
                            count m + 7
                        else
                            count (m*100) + 3 + count d
