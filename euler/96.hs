import Control.Arrow
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO

main = value

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
type Index = (Int, Int)

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
isFailed table = any isEmptyCandidate $ elems table

fill :: Table -> Maybe (Table, Bool)
fill table = case fill1 table of
               next | isSolved next -> Just (next, True)
                    | isFailed next -> Nothing
                    | next == table -> Just (next, False)
                    | otherwise     -> fill next

fill1 = fill1ST

fill1Pure :: Table -> Table
fill1Pure table = fmap fromJust $ foldr f emptyWorkTable (indices table)
  where
    f index workTable = workTable // [(index, Just $ candidates table workTable index)]
    emptyWorkTable = array (bounds table) $ zip (indices table) (repeat Nothing)
    candidates table workTable index =
      case table ! index of
        Fixed x -> Fixed x
        Candidates v -> let indices = relevantIndices (bounds table) index
                            values = nub $ concatMap fixedValue indices
                        in makeValue $ v \\ values
        where
          fixedValue index = case fromMaybe (table ! index) $ workTable ! index of
                               Fixed x -> [x]
                               _       -> []

fill1ST :: Table -> Table
fill1ST table = fmap fromJust $ runSTArray $
  do workTable <- newArray (bounds table) Nothing
     forM_ (indices table) $ \index ->
       candidates workTable index >>= writeArray workTable index . Just
     return workTable
    where
      candidates workTable index =
        case table ! index of
          Fixed x -> return $ Fixed x
          Candidates v -> do let indices = relevantIndices (bounds table) index
                             values <- liftM (nub . concat) $ mapM fixedValue indices
                             return $ makeValue $ v \\ values
          where
            fixedValue index = do v <- readArray workTable index
                                  return $ case fromMaybe (table ! index) v of
                                             Fixed x -> [x]
                                             _       -> []

tryFill :: Table -> [Table]
tryFill table =
  case fill table of
    Just (table, True)  -> [table]
    Just (table, False) -> let index = fromJust $ firstUnfixedIndex table
                               Candidates l = table ! index
                           in concatMap (\x -> tryFill $ table // [(index, Fixed x)]) l
    Nothing             -> []
  where
    firstUnfixedIndex table = fmap fst $ find (not . isFixed . snd) $ assocs table

relevantIndices :: Bounds -> Index -> [Index]
relevantIndices bounds index = 
  delete index $ nub $ (columnIndices bounds `mappend` rowIndices bounds `mappend` boxIndices) index
    where
      rowIndices ((lr, _), (ur, _)) (_, c) = map (\r -> (r, c)) $ [lr..ur]
      columnIndices ((_, lc), (_, uc)) (r, _) = map (\c -> (r, c)) $ [lc..uc]
      boxIndices index@(r, c) = [ (row, column) | row <- indices r, column <- indices c ]
        where
          indices n = take 3 [n - (n `mod` 3)..]
