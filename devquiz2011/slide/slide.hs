{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad (forM, liftM)
import Control.Monad.State (MonadState, evalState, get, modify)
import Data.Array (Array)
import Data.Array.IArray (IArray, Ix, (!), (//), assocs, bounds, elems, listArray)
import Data.List (tails)
import Data.List.Split (sepBy)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (interact)

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
solve (Input hands boards) = map (head . solveBoard) boards

solveBoard :: Board -> [Moves]
--solveBoard board = solveBoard' board Set.empty
solveBoard board = evalState (solveBoardM' board) Set.empty

solveBoard' :: Board -> Set Board -> [Moves]
solveBoard' board@(Board panels emptyIx) cache
    | Set.member board cache = []
    | isBoardSorted board = [[]]
    | otherwise =
        let nextBoards = [ (m, b) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
        in concatMap (\(c, b) -> map (c:) $ solveBoard' b (Set.insert board cache)) nextBoards

solveBoardM' :: MonadState (Set Board) m => Board -> m [Moves]
solveBoardM' board@(Board panels emptyIx) =
    do cache <- get
       case () of
         _ | Set.member board cache -> return []
         _ | isBoardSorted board -> return [[]]
         _ -> do modify (Set.insert board) 
                 let nextBoards = [ (m, b) | (m, Just b) <- map (\m -> (m, move board m)) [L, R, U, D]]
                 liftM concat $ forM nextBoards $ \(m, b) ->
                     liftM (map (m:)) $ solveBoardM' b

isBoardSorted :: Board -> Bool
isBoardSorted (Board panel _) =
    all (\[p1, p2] -> p1 < p2) $ map (take 2) $ takeWhile ((>= 2) . length) $ tails $ filter (/= Wall) $ elems panel

move :: Board -> Move -> Maybe Board
move (Board panels emptyIx) m
    | not $ isValidMove m = Nothing
    | otherwise           =
        let nextEmptyIx = moveIx emptyIx m
        in Just $ Board (panels // [(nextEmptyIx, Empty), (emptyIx, panels ! nextEmptyIx)]) nextEmptyIx
    where
      ((minRow, minColumn), (maxRow, maxColumn)) = bounds panels
      isValidMove m = let (nextRow, nextColumn) = moveIx emptyIx m
                      in minRow <= nextRow && nextRow <= maxRow &&
                         minColumn <= nextColumn && nextColumn <= maxColumn

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
                   panels = listArray ((0, 0), (read w - 1, read h - 1)) $ map parsePanel p
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
