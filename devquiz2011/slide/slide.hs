{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData)
import Control.Monad (liftM)
import Control.Monad.State (MonadState, get, put, runState)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (IArray, Ix, (!), (//), assocs, bounds, elems, listArray)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable (Hashable(..))
import Data.List (sort)
import Data.List.Split (sepBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import qualified Debug.Trace
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

data Input = Input Hands [Board] deriving (Show, Eq)

data Hands = Hands {
    left  :: Int,
    right :: Int,
    up    :: Int,
    down  :: Int
} deriving (Show, Eq)

data Board = Board {
    panels  :: UArray (Int, Int) Panel,
    emptyIx :: (Int, Int),
    h       :: Int
} deriving (Show, Eq)

instance Hashable Board where
    hash (Board _ _ h) = h

makeBoard :: UArray (Int, Int) Panel -> (Int, Int) -> Board
makeBoard panels emptyIx = Board panels emptyIx hashPanels
    where
      ((_, minColumn), (_, maxColumn)) = bounds panels
      hashPanels = sum $ map (uncurry hashPanelWithIx) $ filter ((/= wall) . snd) $ assocs panels
      hashPanelWithIx (r, c) panel = (r * (maxColumn - minColumn + 1) + c) * hashPanel panel

type Panel = Int

wall, empty :: Panel
wall  = 100
empty = 101

isPanel :: Panel -> Bool
isPanel p = p < wall

data Move = L
          | R
          | U
          | D
  deriving (Show, Eq)

instance NFData Move

type Moves = [Move]


main :: IO ()
main = do i:p:mf:df:args <- getArgs
          results <- if null args then
                         return $ repeat []
                     else
                         liftM (map (map parseMove) . lines) $ readFile $ head args
          hSetBuffering stdout LineBuffering
          interact (writeOutput . solveAllBoards (read i) (read p) (read mf) (read df) results . readInput)

solveAllBoards :: Int -> Int -> Int -> Int -> [Moves] -> Input -> [Moves]
solveAllBoards maxIteration maxPriority moveFactor distanceFactor results (Input _ boards) =
    parMap rdeepseq solveSingleBoard $ zip3 boards results [1..]
    where
      solveSingleBoard (b, [],    n) = trace (show n) $ fromMaybe [] $ solveBoard maxIteration maxPriority moveFactor distanceFactor b
      solveSingleBoard (_, moves, n) = trace (show n) $ moves

data Direction = FORWARD
               | BACKWARD
  deriving (Show, Eq)

instance Hashable Direction where
    hash FORWARD  = 7
    hash BACKWARD = 11

data Item = Item Board Moves Direction Board DistanceMap Int deriving Show

priority :: Item -> Int
priority (Item _ _ _ _ _ p) = p

instance Eq Item where
    i1 == i2 = priority i1 == priority i2

instance Ord Item where
    compare i1 i2 = compare (priority i1) (priority i2)

data OpenItems = OpenItems (MinQueue Item) (HashMap (Board, Direction) Item)

emptyOpenItems :: OpenItems
emptyOpenItems = OpenItems PQ.empty HashMap.empty

getNextOpenItem :: OpenItems -> ClosedItems -> Maybe (Item, OpenItems, ClosedItems)
getNextOpenItem (OpenItems q t) (ClosedItems m) =
    case PQ.getMin q of
      Just item@(Item b _ d _ _ _) | isJust $ HashMap.lookup (b, d) t -> Just $ (item, OpenItems (PQ.deleteMin q) (HashMap.delete (b, d) t), ClosedItems (HashMap.insert b item m))
                                   | otherwise                        -> getNextOpenItem (OpenItems (PQ.deleteMin q) t) (ClosedItems m)
      Nothing -> Nothing

getOpenItem :: Board -> Direction -> OpenItems -> Maybe Item
getOpenItem b d (OpenItems _ t) = HashMap.lookup (b, d) t

addOpenItem :: Item -> OpenItems -> OpenItems
addOpenItem item@(Item b _ d _ _ _) (OpenItems q t) = OpenItems (PQ.insert item q) (HashMap.insert (b, d) item t)

removeOpenItem :: Item -> OpenItems -> OpenItems
removeOpenItem (Item b _ d _ _ _) (OpenItems q t) = OpenItems q (HashMap.delete (b, d) t)

newtype ClosedItems = ClosedItems (HashMap Board Item) deriving Show

emptyClosedItems :: ClosedItems
emptyClosedItems = ClosedItems HashMap.empty

getClosedItem :: Board -> ClosedItems -> Maybe Item
getClosedItem b (ClosedItems m) = HashMap.lookup b m

removeClosedItem :: Item -> ClosedItems -> ClosedItems
removeClosedItem (Item b _ _ _ _ _) (ClosedItems m) = ClosedItems $ HashMap.delete b m

getClosedItemsSize :: ClosedItems -> Int
getClosedItemsSize (ClosedItems m) = HashMap.size m

solveBoard :: Int -> Int -> Int -> Int -> Board -> Maybe Moves
solveBoard maxIteration maxPriority moveFactor distanceFactor board =
    let goalBoard = getGoalBoard board
        distanceMap = makeDistanceMap board
        goalDistanceMap = makeDistanceMap goalBoard
        initialItems = [Item board [] FORWARD goalBoard goalDistanceMap (distance goalDistanceMap distanceFactor board),
                        Item goalBoard [] BACKWARD board distanceMap (distance distanceMap distanceFactor goalBoard)]
        (moves, (_, closedItems)) = runState (solveBoard' maxIteration maxPriority moveFactor distanceFactor 0) (foldr addOpenItem emptyOpenItems initialItems, emptyClosedItems)
    in Debug.Trace.trace (show (fmap length moves) ++ ", " ++ show (getClosedItemsSize closedItems)) $ fmap reverse moves

solveBoard' :: MonadState (OpenItems, ClosedItems) m => Int -> Int -> Int -> Int -> Int -> m (Maybe Moves)
solveBoard' maxIteration maxPriority moveFactor distanceFactor n =
    do (openItems, closedItems) <- get
       case getNextOpenItem openItems closedItems of
         _ | n > maxIteration -> return Nothing
         Just (Item board moves direction goal distanceMap priority, nextOpenItems, nextClosedItems) ->
             if direction == FORWARD && board == goal then
                 return $ Just moves
             else if direction == BACKWARD && board == goal then
                 return $ Just $ reverse (map reverseMove moves)
             else
                 case (getClosedItem board closedItems, direction) of
                   (Just (Item _ m BACKWARD _ _ _), FORWARD) -> return $ Just $ reverse (map reverseMove m) ++ moves
                   (Just (Item _ m FORWARD _ _ _), BACKWARD) -> return $ Just $ reverse (map reverseMove moves) ++ m
                   _ -> do let nextItems = [ Item b (m:moves) direction goal distanceMap (priority + 1 * moveFactor - panelDistance distanceMap distanceFactor (emptyIx b) (panels board ! emptyIx b) + panelDistance distanceMap distanceFactor (emptyIx board) (panels b ! emptyIx board)) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D] ]
                           put $ foldr insert (nextOpenItems, nextClosedItems) nextItems
                           solveBoard' maxIteration maxPriority moveFactor distanceFactor $ n + 1
         Nothing -> return Nothing
    where
      insert item@(Item b _ d _ _ p) (openItems, closedItems)
             | p > maxPriority = (openItems, closedItems)
             | otherwise = case (getClosedItem b closedItems, getOpenItem b d openItems) of
                             (Nothing, Nothing) -> (addOpenItem item openItems, closedItems)
                             (Just closedItem@(Item _ _ _ _ _ cp), _) | p < cp -> (addOpenItem item openItems, removeClosedItem closedItem closedItems)
                             (_, Just openItem@(Item _ _ _ _ _ op)) | p < op -> (addOpenItem item $ removeOpenItem openItem openItems, closedItems)
                             (_, _) -> (openItems, closedItems)
      reverseMove L = R
      reverseMove R = L
      reverseMove U = D
      reverseMove D = U

newtype DistanceMap = DistanceMap (Map Panel (UArray (Int, Int) Int)) deriving Show

makeDistanceMap :: Board -> DistanceMap
makeDistanceMap (Board panels _ _) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
        indices = [ (r, c) | r <- [minRow .. maxRow], c <- [minColumn .. maxColumn] ]
        normalPanels = map swap $ filter (isPanel . snd) $ assocs panels
    in DistanceMap $ Map.fromList $ map (\(p, ix) -> (p, listArray (bounds panels) $ map (\i -> dist i ix) indices)) normalPanels
    where
      dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

distance :: DistanceMap -> Int -> Board -> Int
distance distanceMap factor (Board panels _ _) = 
    sum $ map (uncurry $ panelDistance distanceMap factor) $ filter (isPanel . snd) $ assocs panels

panelDistance :: DistanceMap -> Int -> (Int, Int) -> Panel -> Int
panelDistance  (DistanceMap m) factor ix panel = ((fromJust $ Map.lookup panel m) ! ix) * factor

getGoalBoard :: Board -> Board
getGoalBoard (Board panels _ _) =
    let goalPanels = [ p | p <- sort $ elems panels, p /= wall ]
    in makeBoard (listArray (bounds panels) (fill (elems panels) goalPanels)) (snd $ bounds panels)
    where
      fill []         _                           = []
      fill (p:panels) (gp:goalPanels) | p == wall = wall:fill panels (gp:goalPanels)
                                      | otherwise = gp:fill panels goalPanels

move :: Board -> Move -> Maybe Board
move (Board panels emptyIx _) m
    | isValidMove = Just $ makeBoard (panels // [(nextEmptyIx, empty), (emptyIx, panels ! nextEmptyIx)]) nextEmptyIx
    | otherwise   = Nothing
    where
      nextEmptyIx@(nextRow, nextColumn) = moveIx emptyIx m
      ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
      isValidMove = minRow <= nextRow && nextRow <= maxRow &&
                    minColumn <= nextColumn && nextColumn <= maxColumn &&
                    panels ! (nextRow, nextColumn) /= wall

moveIx :: (Int, Int) -> Move -> (Int, Int)
moveIx (r, c) L = (r, c - 1)
moveIx (r, c) R = (r, c + 1)
moveIx (r, c) U = (r - 1, c)
moveIx (r, c) D = (r + 1, c)

findIx :: (IArray a e, Ix i, Eq e) => e -> a i e -> Maybe i
findIx e a = lookup e $ map swap $ assocs a

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
                   Just emptyIx = findIx empty panels
               in makeBoard panels emptyIx

parsePanel :: Char -> Panel
parsePanel '=' = wall
parsePanel '0' = empty
parsePanel c | '1' <= c && c <= '9' = fromEnum c - fromEnum '0'
             | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 10

parseMove :: Char -> Move
parseMove 'L' = L
parseMove 'R' = R
parseMove 'U' = U
parseMove 'D' = D

hashPanel :: Panel -> Int
hashPanel 1 = 101
hashPanel 2 = 103
hashPanel 3 = 107
hashPanel 4 = 109
hashPanel 5 = 113
hashPanel 6 = 127
hashPanel 7 = 131
hashPanel 8 = 137
hashPanel 9 = 139
hashPanel 10 = 149
hashPanel 11 = 151
hashPanel 12 = 157
hashPanel 13 = 163
hashPanel 14 = 167
hashPanel 15 = 173
hashPanel 16 = 179
hashPanel 17 = 181
hashPanel 18 = 191
hashPanel 19 = 193
hashPanel 20 = 197
hashPanel 21 = 199
hashPanel 22 = 211
hashPanel 23 = 223
hashPanel 24 = 227
hashPanel 25 = 229
hashPanel 26 = 233
hashPanel 27 = 239
hashPanel 28 = 241
hashPanel 29 = 251
hashPanel 30 = 257
hashPanel 31 = 263
hashPanel 32 = 269
hashPanel 33 = 271
hashPanel 34 = 277
hashPanel 35 = 281
hashPanel 101 = 283

writeOutput :: [Moves] -> String
writeOutput = unlines . map (map printMove)

printMove :: Move -> Char
printMove L = 'L'
printMove R = 'R'
printMove U = 'U'
printMove D = 'D'

printBoard :: Board -> String
printBoard (Board panels _ _) =
    let ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
    in show (maxRow - minRow + 1) ++ "," ++ show (maxColumn - minColumn + 1) ++ "," ++ map printPanel (elems panels)

printPanel :: Panel -> Char
printPanel p |  1 <= p && p <= 9  = toEnum $ fromEnum '1' + (p - 1)
             | 10 <= p && p <= 35 = toEnum $ fromEnum 'A' + (p - 10)
             | p == wall          = '='
             | p == empty         = '0'


trace :: String -> a -> a
trace = Debug.Trace.trace
--trace _ a = a

play :: Board -> Moves -> Board
play = foldl (\b m -> trace (printBoard b) $ fromJust $ move b m)
