module DateTime where

import System.Time
import Text.Regex.Lazy
import Text.Regex.PCRE ((=~~))

formatDateTime :: ClockTime -> String
formatDateTime time = let t = toUTCTime time
                      in show (ctYear t) ++ "-" ++
                         show (fromEnum (ctMonth t) + 1) ++ "-" ++
                         show (ctDay t) ++ " " ++
                         show (ctHour t) ++ ":" ++
                         show (ctMin t) ++ ":" ++
                         show (ctSec t)

parseDateTime :: String -> ClockTime
parseDateTime s =
    let x :: Maybe (String, String, String, [String])
        x = s =~~ "(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+):(\\d+)"
    in case x of
        Just (_, _, _, m) ->
            let [year, month, day, hour, min, sec] = map read m
                c = CalendarTime {
                        ctYear    = year,
                        ctMonth   = toEnum $ month - 1,
                        ctDay     = day,
                        ctHour    = hour,
                        ctMin     = min,
                        ctSec     = sec,
                        ctPicosec = 0,
                        ctWDay    = Sunday,
                        ctYDay    = 0,
                        ctTZName  = "UTC",
                        ctTZ      = 0,
                        ctIsDST   = False
                }
            in toClockTime c
