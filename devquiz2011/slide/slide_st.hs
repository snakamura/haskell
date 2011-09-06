{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad (foldM, liftM)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (IArray, Ix, (!), (//), assocs, bounds, elems, listArray)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable (Hashable(..))
import qualified Data.HashTable.Class as HashTable
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.List (sort, sortBy, tails)
import Data.List.Split (sepBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (comparing)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Debug.Trace

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
} deriving (Show, Eq, Ord)

instance Hashable Board where
    hash (Board _ _ h) = h

makeBoard :: UArray (Int, Int) Panel -> (Int, Int) -> Board
makeBoard panels emptyIx = Board panels emptyIx (hash panels)

instance (Hashable ix, Ix ix, Hashable a, IArray UArray a) => Hashable (UArray ix a) where
    hash = hash . assocs

type Panel = Int

wall  = 100
empty = 101

isPanel :: Panel -> Bool
isPanel p = p < wall

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

data Direction = FORWARD
               | BACKWARD
  deriving (Show, Eq, Ord)

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

data OpenItems s = OpenItems (MinQueue Item) (HashTable s (Board, Direction) Item) deriving Show

emptyOpenItems :: ST s (OpenItems s)
emptyOpenItems = do t <- HashTable.newSized 1000
                    return $ OpenItems PQ.empty t

getNextOpenItem :: OpenItems s -> ST s (Maybe (Item, OpenItems s))
getNextOpenItem (OpenItems q t) =
    case PQ.getMin q of
      Just item@(Item b _ d _ _ _) ->
          do tableItem <- HashTable.lookup t (b, d)
             case tableItem of
               Just _ -> do HashTable.delete t (b, d)
                            return $ Just $ (item, OpenItems (PQ.deleteMin q) t)
               Nothing -> getNextOpenItem (OpenItems (PQ.deleteMin q) t)
      Nothing -> return Nothing

getOpenItem :: Board -> Direction -> OpenItems s -> ST s (Maybe Item)
getOpenItem b d (OpenItems _ t) = HashTable.lookup t (b, d)

addOpenItem :: Item -> OpenItems s -> ST s (OpenItems s)
addOpenItem item@(Item b _ d _ _ _) (OpenItems q t) = do HashTable.insert t (b, d) item
                                                         return $ OpenItems (PQ.insert item q) t

removeOpenItem :: Item -> OpenItems s -> ST s (OpenItems s)
removeOpenItem item@(Item b _ d _ _ _) (OpenItems q t) = do HashTable.delete t (b, d)
                                                            return $ OpenItems q t

newtype ClosedItems s = ClosedItems (HashTable s Board Item) deriving Show

emptyClosedItems :: ST s (ClosedItems s)
emptyClosedItems = liftM ClosedItems $ HashTable.newSized 1000

getClosedItem :: Board -> ClosedItems s -> ST s (Maybe Item)
getClosedItem b (ClosedItems m) = HashTable.lookup m b

addClosedItem :: Item -> ClosedItems s -> ST s ()
addClosedItem item@(Item b _ _ _ _ _) (ClosedItems m) = HashTable.insert m b item

removeClosedItem :: Item -> ClosedItems s -> ST s ()
removeClosedItem (Item b _ _ _ _ _) (ClosedItems m) = HashTable.delete m b

getClosedItemsSize :: ClosedItems s -> ST s Int
getClosedItemsSize (ClosedItems m) = liftM length $ HashTable.toList m

solveBoard :: Board -> Maybe Moves
solveBoard board = let goalBoard = getGoalBoard board
                       distanceMap = makeDistanceMap board
                       goalDistanceMap = makeDistanceMap goalBoard
                       initialItems = [Item board [] FORWARD goalBoard goalDistanceMap (distance goalDistanceMap board),
                                       Item goalBoard [] BACKWARD board distanceMap (distance distanceMap goalBoard)]
                       (moves, closedItemsLength) = runST $ do initialOpenItems <- emptyOpenItems
                                                               initialOpenItems <- foldM (flip addOpenItem) initialOpenItems initialItems
                                                               initialClosedItems <- emptyClosedItems
                                                               state <- newSTRef (initialOpenItems, initialClosedItems)
                                                               moves <- solveBoard' state 0
                                                               (o, c) <- readSTRef state
                                                               cl <- getClosedItemsSize c
                                                               return (moves, cl)
                   in Debug.Trace.trace (show (fmap length moves) ++ ", " ++ show closedItemsLength) $ fmap reverse moves

solveBoard' :: STRef s (OpenItems s, ClosedItems s) -> Int -> ST s (Maybe Moves)
solveBoard' state n =
    do (openItems, closedItems) <- readSTRef state
       nextOpenItem <- getNextOpenItem openItems
       case nextOpenItem of
         _ | n > 2000 -> return Nothing
--         _ | getClosedItemsSize closedItems > 2000 -> return Nothing
         Just (item@(Item board moves direction goal distanceMap priority), nextOpenItems) ->
             if direction == FORWARD && board == goal then
                 return $ Just moves
             else if direction == BACKWARD && board == goal then
                 return $ Just $ reverse (map reverseMove moves)
             else
                 do closedItem <- getClosedItem board closedItems
                    addClosedItem item closedItems
                    case (closedItem, direction) of
                      (Just (Item _ m BACKWARD _ _ _), FORWARD) -> return $ Just $ reverse (map reverseMove m) ++ moves
                      (Just (Item _ m FORWARD _ _ _), BACKWARD) -> return $ Just $ reverse (map reverseMove moves) ++ m
                      _ -> do let nextItems = [ Item b (m:moves) direction goal distanceMap (priority + 1 - panelDistance distanceMap (emptyIx b) (panels board ! emptyIx b) + panelDistance distanceMap (emptyIx board) (panels b ! emptyIx board)) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D] ]
                              newState <- foldM insert (nextOpenItems, closedItems) nextItems
                              writeSTRef state $ newState
                              solveBoard' state $ n + 1
         Nothing -> return Nothing
    where
      insert (openItems, closedItems) item@(Item b m d g dm p)
             | p > 60 = return (openItems, closedItems)
             | otherwise = do closedItem <- getClosedItem b closedItems
                              openItem <- getOpenItem b d openItems
                              case (closedItem, openItem) of
                                (Nothing, Nothing) -> do newOpenItems <- addOpenItem item openItems
                                                         return (newOpenItems, closedItems)
                                (Just closedItem@(Item _ _ cd _ _ cp), _) | p < cp -> do removeClosedItem closedItem closedItems
                                                                                         newOpenItems <- addOpenItem item openItems
                                                                                         return (newOpenItems, closedItems)
                                (_, Just openItem@(Item _ _ od _ _ op)) | p < op -> do newOpenItems <- removeOpenItem openItem openItems >>= addOpenItem item
                                                                                       return (newOpenItems, closedItems)
                                (_, _) -> return (openItems, closedItems)
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

distance :: DistanceMap -> Board -> Int
distance distanceMap (Board panels _ _) = 
    sum $ map (uncurry $ panelDistance distanceMap) $ filter (isPanel . snd) $ assocs panels

panelDistance :: DistanceMap -> (Int, Int) -> Panel -> Int
panelDistance  (DistanceMap m) ix panel = (fromJust $ Map.lookup panel m) ! ix
{-
isGoalBoard :: Board -> Bool
isGoalBoard (Board panels _ _) =
    all (\[p1, p2] -> p1 < p2) $ map (take 2) $ takeWhile ((>= 2) . length) $ tails $ filter (/= wall) $ elems panels
-}
getGoalBoard :: Board -> Board
getGoalBoard (Board panels _ _) =
    let goalPanels = [ p | p <- sort $ elems panels, p /= wall ]
    in makeBoard (listArray (bounds panels) (fill (elems panels) goalPanels)) (snd $ bounds panels)
    where
      fill []         _                           = []
      fill (p:panels) (gp:goalPanels) | p == wall = wall:fill panels (gp:goalPanels)
                                      | otherwise = gp:fill panels goalPanels

transposeBoard :: Board -> Board
transposeBoard (Board panels (emptyRow, emptyColumn) _) = makeBoard (transpose panels) (emptyColumn, emptyRow)

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
                   Just emptyIx = findIx empty panels
               in makeBoard panels emptyIx

parsePanel :: Char -> Panel
parsePanel '=' = wall
parsePanel '0' = empty
parsePanel c | '1' <= c && c <= '9' = fromEnum c - fromEnum '0'
             | 'A' <= c && c <= 'Z' = fromEnum c - fromEnum 'A' + 10

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
