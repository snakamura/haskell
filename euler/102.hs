import Control.Monad
import Data.List.Split

type Triangle = [(Int, Int)]

value = 
  do triangles <- liftM (map readTriangle . lines) $ readFile "102.txt"
     print $ length $ filter id $ map containsOrigin triangles

readTriangle :: String -> Triangle
readTriangle s = let [x1, y1, x2, y2, x3, y3] = map read $ sepBy "," s
                 in [(x1, y1), (x2, y2), (x3, y3)]

containsOrigin :: Triangle -> Bool
containsOrigin [(x1, y1), (x2, y2), (x3, y3)] =
  let (_, _, c1) = cross (x1, y1, 0) (x2, y2, 0)
      (_, _, c2) = cross (x2, y2, 0) (x3, y3, 0)
      (_, _, c3) = cross (x3, y3, 0) (x1, y1, 0)
  in (c1 > 0 && c2 > 0 && c3 > 0) || (c1 < 0 && c2 < 0 && c3 < 0)

cross :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
cross (x1, y1, z1) (x2, y2, z2) =
  (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)
