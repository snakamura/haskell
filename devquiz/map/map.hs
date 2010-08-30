import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Network.HTTP
import System.Environment
import Text.JSON


data Point = Point {
    name      :: String,
    latitude  :: String,
    longitude :: String
} deriving (Eq, Show)


main :: IO ()
main = do args <- getArgs
          points <- liftM parsePoints $ readFile $ head args
          edges <- forM [ (o, d) | o <- points, d <- points, o /= d ] $ \(o, d) ->
                       do duration <- duration o d
                          return ((name o, name d), duration)
          let start = head points
              courses = map (\v -> start:v ++ [start]) $ permutations (tail points)
          putStrLn $ unwords $ map name $ snd $ head $ sortBy (comparing fst) $ map (\course -> (courseDuration edges course, course)) courses


courseDuration :: [((String, String), Int)] -> [Point] -> Int
courseDuration edges (o:d:r) =
    fromJust (lookup (name o, name d) edges) + courseDuration edges (d:r)
courseDuration _ _ = 0


parsePoints :: String -> [Point]
parsePoints = map parsePoint . lines

parsePoint :: String -> Point
parsePoint s = let [name, latitude, longitude] = words s
               in Point name latitude longitude


duration :: Point -> Point -> IO Int
duration origin destination =
    do let url = "http://maps.google.com/maps/api/directions/json?origin=" ++
                 latitude origin ++ "," ++ longitude origin ++ "&destination=" ++
                 latitude destination ++ "," ++ longitude destination ++ "&sensor=false"
       response <- simpleHTTP (getRequest url)
       Ok directions <- liftM decode $ getResponseBody response
       let Just (JSArray (JSObject route:_)) = lookup "routes" $ fromJSObject directions
           Just (JSArray (JSObject leg:_)) = lookup "legs" $ fromJSObject route
           Just (JSObject duration) = lookup "duration" $ fromJSObject leg
           Just (JSRational _ value) = lookup "value" $ fromJSObject duration
       return $ floor value
