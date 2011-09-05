{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad.State (MonadState, get, put, runState)
import Data.Array (Array)
import Data.Array.IArray (IArray, Ix, (!), (//), assocs, bounds, elems, listArray)
import Data.List (sort, sortBy, tails)
import Data.List.Split (sepBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import qualified Debug.Trace

data Input = Input {
    hands  :: Hands,
    boards :: [Board]
} deriving (Show, Eq)

data Hands = Hands {
    left  :: Int,
    right :: Int,
    up    :: Int,
    down  :: Int
} deriving (Show, Eq)

data Board = Board {
    panels  :: Array (Int, Int) Panel,
    emptyIx :: (Int, Int)
} deriving (Show, Eq, Ord)

data Panel = Panel Char
           | Wall
           | Empty
  deriving (Show, Eq, Ord)

data Move = L
          | R
          | U
          | D
  deriving (Show, Eq)

type Moves = [Move]


main :: IO ()
main = interact (writeOutput . s . readInput)
    where
      s (Input hands boards) = map solve boards
          

solve :: Board -> Moves
solve board = fromMaybe [] $ solveBoard board
{-
    where
      s board@(Board panel _)
          | r <= 3 && c <= 3 = solveBoard board
          | c * r <= 12 = solveBoardD board
          | otherwise        = Nothing
          where
            (r, c) = let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panel
                     in (maxRow - minRow + 1, maxColumn - minColumn + 1)
-}

data Direction = FORWARD
               | BACKWARD
  deriving (Show, Eq, Ord)

data Item = Item Board Moves Direction Board Int
  deriving Show

makeItem :: Board -> Moves -> Direction -> Board -> Item
makeItem b m d g = Item b m d g (length m + distance b g)

priority :: Item -> Int
priority (Item _ _ _ _ p) = p

instance Eq Item where
    i1 == i2 = priority i1 == priority i2

instance Ord Item where
    compare i1 i2 = compare (priority i1) (priority i2)

data OpenItems = OpenItems (MinQueue Item) (Map (Board, Direction) Item)

emptyOpenItems :: OpenItems
emptyOpenItems = OpenItems PQ.empty Map.empty

getNextOpenItem :: OpenItems -> ClosedItems -> Maybe (Item, OpenItems, ClosedItems)
getNextOpenItem (OpenItems q t) (ClosedItems m) =
    case PQ.getMin q of
      Just item@(Item b _ d _ _) | Map.member (b, d) t -> Just $ (item, OpenItems (PQ.deleteMin q) (Map.delete (b, d) t), ClosedItems (Map.insert b item m))
                                 | otherwise           -> getNextOpenItem (OpenItems (PQ.deleteMin q) t) (ClosedItems m)
      Nothing -> Nothing

getOpenItem :: Board -> Direction -> OpenItems -> Maybe Item
getOpenItem b d (OpenItems _ t) = Map.lookup (b, d) t

addOpenItem :: Item -> OpenItems -> OpenItems
addOpenItem item@(Item b _ d _ _) (OpenItems q t) = OpenItems (PQ.insert item q) (Map.insert (b, d) item t)

removeOpenItem :: Item -> OpenItems -> OpenItems
removeOpenItem item@(Item b _ d _ _) (OpenItems q t) = OpenItems q (Map.delete (b, d) t)

newtype ClosedItems = ClosedItems (Map Board Item) deriving Show

emptyClosedItems :: ClosedItems
emptyClosedItems = ClosedItems Map.empty

getClosedItem :: Board -> ClosedItems -> Maybe Item
getClosedItem b (ClosedItems m) = Map.lookup b m

removeClosedItem :: Item -> ClosedItems -> ClosedItems
removeClosedItem (Item b _ _ _ _) (ClosedItems m) = ClosedItems $ Map.delete b m

getClosedItemsSize :: ClosedItems -> Int
getClosedItemsSize (ClosedItems m) = Map.size m

solveBoard :: Board -> Maybe Moves
solveBoard board = let goalBoard = getGoalBoard board
                       initialItems = [makeItem board [] FORWARD goalBoard,
                                       makeItem goalBoard [] BACKWARD board]
                       (moves, (_, closedItems)) = runState solveBoard' (foldr addOpenItem emptyOpenItems initialItems, emptyClosedItems)
                   in Debug.Trace.trace (show (fmap length moves) ++ ", " ++ show (getClosedItemsSize closedItems)) $ fmap reverse moves

solveBoard' :: MonadState (OpenItems, ClosedItems) m => m (Maybe Moves)
solveBoard' =
    do (openItems, closedItems) <- get
       case getNextOpenItem openItems closedItems of
--         _ | getClosedItemsSize closedItems > 2000 -> return Nothing
         Just (item@(Item board moves direction goal priority), nextOpenItems, nextClosedItems) ->
             if direction == FORWARD && board == goal then
                 return $ Just moves
             else if direction == BACKWARD && board == goal then
                 return $ Just $ reverse (map reverseMove moves)
             else
                 case (getClosedItem board closedItems, direction) of
                   (Just (Item _ m BACKWARD _ _), FORWARD) -> return $ Just $ reverse (map reverseMove m) ++ moves
                   (Just (Item _ m FORWARD _ _), BACKWARD) -> return $ Just $ reverse (map reverseMove moves) ++ m
                   _ -> do let nextItems = [ makeItem b (m:moves) direction goal | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D] ]
                           put $ foldr insert (nextOpenItems, nextClosedItems) nextItems
                           solveBoard'
         Nothing -> return Nothing
    where
      insert item@(Item b m d g p) (openItems, closedItems)
             | p > 20 = (openItems, closedItems)
             | otherwise =
          case (getClosedItem b closedItems, getOpenItem b d openItems) of
            (Nothing, Nothing) -> (addOpenItem item openItems, closedItems)
            (Just closedItem@(Item _ _ cd _ cp), _) | p < cp -> (addOpenItem item openItems, removeClosedItem closedItem closedItems)
            (_, Just openItem@(Item _ _ od _ op)) | p < op -> (addOpenItem item $ removeOpenItem openItem openItems, closedItems)
            (_, _) -> (openItems, closedItems)
      reverseMove L = R
      reverseMove R = L
      reverseMove U = D
      reverseMove D = U
{-
solveBoardD :: Board -> Maybe Moves
solveBoardD board@(Board panel _)
    | r <= 4 && c <= 4 = solveBoard board
    | r >= c = let (m, b) = solveFirstRow board
               in case solveBoardD b of
                    Just m2 -> Just $ m ++ m2
                    Nothing -> solveBoard board
    | otherwise = let (m, b) = solveFirstColumn board
                  in case solveBoardD b of
                       Just m2 -> Just $ m ++ m2
                       Nothing -> solveBoard board
    where
      (r, c) = let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panel
               in (maxRow - minRow + 1, maxColumn - minColumn + 1)

data ItemFR = ItemFR Board Moves Board Int

makeItemFR :: Board -> Moves -> Board -> ItemFR
makeItemFR b m g = ItemFR b m g (length m + distanceFR b g)

priorityFR :: ItemFR -> Int
priorityFR (ItemFR _ _ _ p) = p

distanceFR :: Board -> Board -> Int
distanceFR (Board panel _) (Board goalPanel _) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panel
        indices = zip (repeat minRow) [minColumn .. maxColumn]
        goalPanels = [ (p, ix) | ix <- indices, let p = panel ! ix, isPanel p ]
        panels = sortBy (comparing fst) [ (p, ix) | (ix, p@(Panel _)) <- assocs panel, p `elem` map fst goalPanels ]
    in sum $ zipWith dist (map snd panels) (map snd goalPanels)
    where
      isPanel (Panel _) = True
      isPanel _         = False
      dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

instance Eq ItemFR where
    i1 == i2 = priorityFR i1 == priorityFR i2

instance Ord ItemFR where
    compare i1 i2 = compare (priorityFR i1) (priorityFR i2)

solveFirstRow :: Board -> (Moves, Board)
solveFirstRow board = solveFirstRow2 board (getGoalBoard board)

solveFirstRow2 :: Board -> Board -> (Moves, Board)
solveFirstRow2 board goal = let ((moves, b), (_, cache)) = runState solveFirstRow' (PQ.fromList [makeItemFR board [] goal], Map.empty)
                            in trace (show $ Map.size cache) $ (reverse moves, b)

solveFirstRow' :: MonadState (MinQueue ItemFR, Map Board ItemFR) m => m (Moves, Board)
solveFirstRow' =
    do (queue, cache) <- get
       let (item@(ItemFR board moves goal _), newQueue) = PQ.deleteFindMin queue -- Assume non-empty queue
       case Map.lookup board cache of
         _ | isFirstRowGoal board goal -> return (moves, removeFirstRow board)
         Just (ItemFR _ m _ _) -> do put (newQueue, cache)
                                     solveFirstRow'
         Nothing ->
               do let items = [ makeItemFR b (m:moves) goal | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                  put (foldr PQ.insert newQueue items, Map.insert board item cache)
                  trace (printBoard board) $ solveFirstRow'

isFirstRowGoal :: Board -> Board -> Bool
isFirstRowGoal (Board panel _) (Board goalPanel _) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panel
        indices = zip (repeat minRow) [minColumn .. maxColumn]
    in map (panel !) indices == map (goalPanel !) indices

removeFirstRow :: Board -> Board
removeFirstRow (Board panel (emptyRow, emptyColumn)) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panel
    in Board (listArray ((minRow, minColumn), (maxRow - 1, maxColumn)) $ drop (maxColumn - minColumn + 1) $ elems panel) (emptyRow - 1, emptyColumn)

solveFirstColumn :: Board -> (Moves, Board)
solveFirstColumn board = let (moves, b) = solveFirstRow2 (transposeBoard board) (transposeBoard $ getGoalBoard board)
                         in (map transposeMove moves, transposeBoard b)
-}
distance :: Board -> Board -> Int
distance b1 b2 = sum $ zipWith dist (indices b1) (indices b2)
    where
      indices (Board panel _) = map snd $ sortBy (comparing fst) [ (p, ix) | (ix, p@(Panel _)) <- assocs panel ]
      dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

isGoalBoard :: Board -> Bool
isGoalBoard (Board panel _) =
    all (\[p1, p2] -> p1 < p2) $ map (take 2) $ takeWhile ((>= 2) . length) $ tails $ filter (/= Wall) $ elems panel

getGoalBoard :: Board -> Board
getGoalBoard (Board panels _) =
    let goalPanels = [ p | p <- sort $ elems panels, p /= Wall ]
    in Board (listArray (bounds panels) (fill (elems panels) goalPanels)) (snd $ bounds panels)
    where
      fill []            _               = []
      fill (Wall:panels) goalPanels      = Wall:fill panels goalPanels
      fill (_:panels)    (gp:goalPanels) = gp:fill panels goalPanels

transposeBoard :: Board -> Board
transposeBoard (Board panel (emptyRow, emptyColumn)) = Board (transpose panel) (emptyColumn, emptyRow)

move :: Board -> Move -> Maybe Board
move (Board panels emptyIx) m
    | isValidMove = Just $ Board (panels // [(nextEmptyIx, Empty), (emptyIx, panels ! nextEmptyIx)]) nextEmptyIx
    | otherwise   = Nothing
    where
      nextEmptyIx@(nextRow, nextColumn) = moveIx emptyIx m
      ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
      isValidMove = minRow <= nextRow && nextRow <= maxRow &&
                    minColumn <= nextColumn && nextColumn <= maxColumn &&
                    panels ! (nextRow, nextColumn) /= Wall

moveIx :: (Int, Int) -> Move -> (Int, Int)
moveIx (r, c) L = (r, c - 1)
moveIx (r, c) R = (r, c + 1)
moveIx (r, c) U = (r - 1, c)
moveIx (r, c) D = (r + 1, c)

transposeMove :: Move -> Move
transposeMove L = U
transposeMove R = D
transposeMove U = L
transposeMove D = R

findIx :: (IArray a e, Ix i, Eq e) => e -> a i e -> Maybe i
findIx e a = lookup e $ map swap $ assocs a

transpose :: (IArray a1 e, IArray a2 e, Ix ix, Ix iy) => a1 (ix, iy) e -> a2 (iy, ix) e
transpose a = let ((minX, minY), (maxX, maxY)) = bounds a
                  values = map snd $ sortBy (comparing fst) $ map (\((x, y), v) -> ((y, x), v)) $ assocs a
              in listArray ((minY, minX), (maxY, maxX)) values

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

readInput :: String -> Input
readInput s = let inputs = lines s
                  [l, r, u, d] = map read $ words $ head inputs
                  boards = map parseBoard $ drop 2 inputs
              in Input (Hands l r u d) boards

parseBoard :: String -> Board
parseBoard l = let [w, h, p] = sepBy "," l
                   panels = listArray ((0, 0), (read h - 1, read w - 1)) $ map parsePanel p
                   Just emptyIx = findIx Empty panels
               in Board panels emptyIx

parsePanel :: Char -> Panel
parsePanel '=' = Wall
parsePanel '0' = Empty
parsePanel c   = Panel c

writeOutput :: [Moves] -> String
writeOutput = unlines . map (map printMove)

printMove :: Move -> Char
printMove L = 'L'
printMove R = 'R'
printMove U = 'U'
printMove D = 'D'

printBoard :: Board -> String
printBoard (Board panels _) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
    in show (maxRow - minRow + 1) ++ "," ++ show (maxColumn - minColumn + 1) ++ "," ++ map printPanel (elems panels)

printPanel :: Panel -> Char
printPanel (Panel c) = c
printPanel Wall      = '='
printPanel Empty     = '0'


trace :: String -> a -> a
trace = Debug.Trace.trace
--trace _ a = a

play :: Board -> Moves -> Board
play = foldl (\b m -> trace (printBoard b) $ fromJust $ move b m)
