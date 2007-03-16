import Data.List (dropWhile, intersperse, takeWhile, unfoldr)
import Data.Time (Day, addDays, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Calendar.WeekDate (toWeekDate)

printMonth :: Integer -> Int -> IO ()
printMonth year month = mapM_ putStrLn $ formatDays month $ daysOfMonth year month

formatDays :: Int -> [[Day]] -> [String]
formatDays month = map $ (concat . intersperse " " . map (formatDay . toGregorian))
 where
     formatDay (_, m, d) | m == month = show2 d
                         | otherwise  = "  "

show2 :: (Num a, Ord a, Show a) => a -> String
show2 n | n < 10 = ' ':show n
        | True   = show n

daysOfMonth :: Integer -> Int -> [[Day]]
daysOfMonth year month =
    takeWhile ((<= lastDay) . head)
        $ unfoldr (Just . splitAt 7)
        $ dropWhile ((/=0) . snd . sundayStartWeek)
        $ iterate (addDays 1)
        $ addDays (-6) $ fromGregorian year month 1
 where
     lastDay = fromGregorian year month $ gregorianMonthLength year month
