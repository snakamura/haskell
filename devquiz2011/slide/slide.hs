{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad (forM, liftM)
import Control.Monad.State (MonadState, evalState, get, modify, put, runState)
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
import Data.Sequence (Seq, ViewL((:<)), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace

data Input = Input {
    hands :: Hands,
    boards :: [Board]
} deriving (Show, Eq)

data Hands = Hands {
    left :: Int,
    right :: Int,
    up :: Int,
    down :: Int
} deriving (Show, Eq)

data Board = Board {
    panels :: Array (Int, Int) Panel,
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
main = interact (writeOutput . solve . readInput)

solve :: Input -> [Moves]
solve (Input hands boards) = map (fromMaybe [] . solveBoardD) boards

solveBoard :: Board -> [Moves]
solveBoard board = solveBoard' board Set.empty

solveBoard' :: Board -> Set Board -> [Moves]
solveBoard' board@(Board panels emptyIx) cache
    | Set.member board cache = []
    | isGoalBoard board = [[]]
    | otherwise =
        let nextBoards = [ (m, b) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
        in concatMap (\(c, b) -> map (c:) $ solveBoard' b (Set.insert board cache)) nextBoards

solveBoardM :: Board -> [Moves]
solveBoardM board = evalState (solveBoardM' board) Set.empty

solveBoardM' :: MonadState (Set Board) m => Board -> m [Moves]
solveBoardM' board@(Board panels emptyIx) =
    do cache <- get
       case () of
         _ | Set.member board cache -> return []
           | isGoalBoard board -> return [[]]
           | otherwise ->
               do modify (Set.insert board) 
                  let nextBoards = [ (m, b) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                  liftM concat $ forM nextBoards $ \(m, b) ->
                      liftM (map (m:)) $ solveBoardM' b

data Item = Item Board Moves

solveBoardQ :: Board -> Moves
solveBoardQ board = reverse $ evalState solveBoardQ' (Seq.singleton (Item board []), Set.empty)

solveBoardQ' :: MonadState (Seq Item, Set Board) m => m Moves
solveBoardQ' =
    do (queue, cache) <- get
       let (Item board moves) :< newQueue = Seq.viewl queue -- Assume non-empty queue
       case () of
         _ | Set.member board cache ->
               do put (newQueue, cache)
                  solveBoardQ'
           | isGoalBoard board -> return moves
           | otherwise ->
               do let items = [ Item b (m:moves) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                  put (newQueue >< Seq.fromList items, Set.insert board cache)
                  solveBoardQ'

data Direction = FORWARD
               | BACKWARD
  deriving (Eq)

data ItemB = ItemB Board Moves Direction

solveBoardQB :: Board -> Moves
solveBoardQB board = let (moves, (_, cache)) = runState solveBoardQB' (Seq.fromList [ItemB board [] FORWARD, ItemB (goalBoard board) [] BACKWARD], Map.empty)
                     in trace (show $ Map.size cache) $ reverse moves

solveBoardQB' :: MonadState (Seq ItemB, Map Board ItemB) m => m Moves
solveBoardQB' =
    do (queue, cache) <- get
       let (ItemB board moves direction) :< newQueue = Seq.viewl queue -- Assume non-empty queue
       case trace (printBoard board) $ Map.lookup board cache of
         _ | direction == FORWARD && isGoalBoard board -> return moves
         Just item@(ItemB _ m d)
             | d == direction ->
                 do put (newQueue, cache)
                    solveBoardQB'
             | direction == FORWARD -> return $ reverse m ++ moves
             | direction == BACKWARD -> return $ reverse moves ++ m
         Nothing ->
               do let items = [ ItemB b (m:moves) direction | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                  put (newQueue >< Seq.fromList items, Map.insert board (ItemB board moves direction) cache)
                  solveBoardQB'

data ItemPQB = ItemPQB Board Moves Direction Board Int

makeItemPQB :: Board -> Moves -> Direction -> Board -> ItemPQB
makeItemPQB b m d g = ItemPQB b m d g (length m + distance b g)

priorityPQB :: ItemPQB -> Int
priorityPQB (ItemPQB _ _ _ _ p) = p

instance Eq ItemPQB where
    i1 == i2 = priorityPQB i1 == priorityPQB i2

instance Ord ItemPQB where
    compare i1 i2 = compare (priorityPQB i1) (priorityPQB i2)

solveBoardPQB :: Board -> Maybe Moves
solveBoardPQB board = let goal = goalBoard board
                          (moves, (_, cache)) = runState solveBoardPQB' (PQ.fromList [makeItemPQB board [] FORWARD goal, makeItemPQB goal [] BACKWARD board], Map.empty)
                      in trace (show $ Map.size cache) $ fmap reverse moves

solveBoardPQB' :: MonadState (MinQueue ItemPQB, Map Board ItemPQB) m => m (Maybe Moves)
solveBoardPQB' =
    do (queue, cache) <- get
       case PQ.getMin queue of
         Just item@(ItemPQB board moves direction goal _) ->
             do let newQueue = PQ.deleteMin queue
                case (Map.lookup board cache, direction) of
                  (_, FORWARD) | isGoalBoard board -> return $ Just moves
                  (Just (ItemPQB _ m d _ _), direction) | d == direction ->
                                                            do put (newQueue, cache)
                                                               solveBoardPQB'
                  (Just (ItemPQB _ m _ _ _), FORWARD) -> return $ Just $ reverse (map reverseMove m) ++ moves
                  (Just (ItemPQB _ m _ _ _), BACKWARD) -> return $ Just $ reverse (map reverseMove moves) ++ m
                  (Nothing, _) ->
                      do let items = [ makeItemPQB b (m:moves) direction goal | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                         put (foldr PQ.insert newQueue items, Map.insert board item cache)
                         trace (printBoard board) $ solveBoardPQB'
         Nothing -> return Nothing
    where
      reverseMove L = R
      reverseMove R = L
      reverseMove U = D
      reverseMove D = U

solveBoardD :: Board -> Maybe Moves
solveBoardD board@(Board panel _)
    | r <= 4 && c <= 4 = solveBoardPQB board
    | otherwise = Nothing
    | r >= c = let (m, b) = solveFirstRow board
               in case solveBoardD b of
                    Just m2 -> Just $ m ++ m2
                    Nothing -> solveBoardPQB board
    | otherwise = let (m, b) = solveFirstColumn board
                  in case solveBoardD b of
                       Just m2 -> Just $ m ++ m2
                       Nothing -> solveBoardPQB board
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
solveFirstRow board = solveFirstRow2 board (goalBoard board)

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
solveFirstColumn board = let (moves, b) = solveFirstRow2 (transposeBoard board) (transposeBoard $ goalBoard board)
                         in (map transposeMove moves, transposeBoard b)

distance :: Board -> Board -> Int
distance b1 b2 = sum $ zipWith dist (indices b1) (indices b2)
    where
      indices (Board panel _) = map snd $ sortBy (comparing fst) [ (p, ix) | (ix, p@(Panel _)) <- assocs panel ]
      dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

isGoalBoard :: Board -> Bool
isGoalBoard (Board panel _) =
    all (\[p1, p2] -> p1 < p2) $ map (take 2) $ takeWhile ((>= 2) . length) $ tails $ filter (/= Wall) $ elems panel

goalBoard :: Board -> Board
goalBoard (Board panels _) =
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
--trace = Debug.Trace.trace
trace _ a = a

play :: Board -> Moves -> Board
play = foldl (\b m -> trace (printBoard b) $ fromJust $ move b m)
