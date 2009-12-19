import Control.Arrow
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO


value =
  do tables <- liftM (map solve . loadTables) $ readFile "96.txt"
     print $ sum $ map num tables
    where
      num table = (v (table ! (0, 0)) * 100) + (v (table ! (0, 1)) * 10) + v (table ! (0, 2))
      v (Fixed x) = x

loadTables :: String -> [Table]
loadTables = map (readTable . unlines . tail) . splitN 10 . lines

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n l  = take n l:splitN n (drop n l)


data Value = Fixed Int
           | Candidates [Int]
  deriving (Eq, Show)
           
makeValue :: [Int] -> Value
makeValue [x] = Fixed x
makeValue l   = Candidates l

isFixed :: Value -> Bool
isFixed (Fixed _) = True
isFixed _         = False

isEmptyCandidate :: Value -> Bool
isEmptyCandidate (Candidates []) = True
isEmptyCandidate _               = False


type Table = Array (Int, Int) Value
type Bounds = ((Int, Int), (Int, Int))

solveFile :: String -> IO ()
solveFile path =
  do table <- liftM readTable $ readFile path
     mapM_ (putStrLn . showTable) $ tryFill table

readTable :: String -> Table
readTable table =
  listArray ((0, 0), (8, 8)) $ ((map (value . read . (:[]))) . concat . lines) table
    where
      value x | x == 0    = Candidates [1..9]
              | otherwise = Fixed x

showTable :: Table -> String
showTable table =
  let ((lr, lc), (ur, uc)) = bounds table
  in concatMap (\r -> concatMap (\c -> showValue (table ! (r, c))) [lc..uc] ++ "\n") [lr..ur]
     
showValue :: Value -> String
showValue (Fixed x)      = show x
showValue (Candidates l) = show l


solve :: Table -> Table
solve = head . tryFill

isSolved :: Table -> Bool
isSolved table = all isFixed $ elems table

isFailed :: Table -> Bool
isFailed table = all isEmptyCandidate $ elems table

fill :: Table -> Table
fill table = let next = fill1 table
             in if isSolved next || isFailed next || next == table then
                  next
                else
                  fill next

fill1 :: Table -> Table
fill1 table = listArray (bounds table) $ map (candidates table) (indices table)

tryFill :: Table -> [Table]
tryFill table =
  case fill table of
    table | isSolved table && isValid table -> [table]
          | isSolved table -> []
          | isFailed table -> []
          | otherwise      -> let index = fromJust $ firstUnfixedIndex table
                                  Candidates l = table ! index
                              in concatMap (\x -> tryFill $ table // [(index, Fixed x)]) l

isValid :: Table -> Bool
isValid table = all (isValidCell table) $ indices table

isValidCell :: Table -> (Int, Int) -> Bool
isValidCell table index = let indices = [rowIndices (bounds table) index,
                                         columnIndices (bounds table) index,
                                         boxIndices index]
                              values = map f indices
                          in all isValidBlock values
                           where
                             f indices = let values = map (table !) indices
                                         in map v values
                             v (Fixed x) = x

isValidBlock :: [Int] -> Bool
isValidBlock block = sort block == [1..9]


firstUnfixedIndex :: Table -> Maybe (Int, Int)
firstUnfixedIndex table = fmap fst $ find (not . isFixed . snd) $ assocs table

candidates :: Table -> (Int, Int) -> Value
candidates table index = 
  case table ! index of
    Fixed x -> Fixed x
    Candidates v -> let indices = relevantIndices (bounds table) index
                        values = nub $ concatMap fixedValue indices
                    in makeValue $ v \\ values
    where
      fixedValue index = case table ! index of
                           Fixed x -> [x]
                           _       -> []

relevantIndices :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
relevantIndices bounds index@(r, c) = 
  delete index $ nub $ ((columnIndices bounds) `mappend` (rowIndices bounds) `mappend` boxIndices) index

rowIndices :: Bounds -> (Int, Int) -> [(Int, Int)]
rowIndices ((lr, _), (ur, _)) (_, c) = map (\r -> (r, c)) $ [lr..ur]

columnIndices :: Bounds -> (Int, Int) -> [(Int, Int)]
columnIndices ((_, lc), (_, uc)) (r, _) = map (\c -> (r, c)) $ [lc..uc]

boxIndices :: (Int, Int) -> [(Int, Int)]
boxIndices index@(r, c) =
  [ (row, column) | row <- indices r, column <- indices c ]
    where
      indices n = take 3 [n - (n `mod` 3)..]
