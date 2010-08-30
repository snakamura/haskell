import Control.Monad
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import System.Environment

data Cell = Wall
          | Empty
          | Dot
  deriving (Show, Eq)

type Position = (Int, Int)

data Object = Object ObjectType -- Type
                     Position   -- Position
                     Position   -- Previous position
  deriving Show

data ObjectType = V
                | H
                | L
                | R
                | J Bool
  deriving Show  

type Field = Array Position Cell

data Step = Step Field Position [Object] deriving Show

main :: IO ()
main = do args <- getArgs
          (time, step) <- liftM load $ readFile $ head args
          let s = head $ filter (\((Step field _ _), _) -> dots field == 0) $ apply (time - 1) $ first step
          mapM_ putStrLn $ showStep $ fst s
          putStrLn $ reverse $ snd s
          print $ length $ snd s

apply :: Int -> [(Step, String)] -> [(Step, String)]
apply 0 steps = steps
apply n steps = filter (\((Step field _ _), _) -> dots field <= n) $ apply (n - 1) (concatMap next steps)

first :: Step -> [(Step, String)]
first step@(Step field position objects) =
    let nextSelfs = filter (not . wall field . fst) $ candidates position
        nextObjects = map move objects
    in filter (not . crash step . fst) $ map (\(self, p) -> (Step (makeEmpty field self) self nextObjects, [p])) nextSelfs
    where
      move (Object t position _) =
          let nextPosition = head $ filter (not . wall field) $ allNextCells position
          in Object t nextPosition position

next :: (Step, String) -> [(Step, String)]
next (step@(Step field position@(sr, sc) objects), path) =
    let nextSelfs = filter (not . wall field . fst) $ candidates position
        nextObjects = map move objects
    in filter (not . crash step . fst) $ map (\(self, p) -> (Step (makeEmpty field self) self nextObjects, p:path)) nextSelfs
    where
      move object@(Object t position previousPosition) =
          case wallCount field position of
            3 -> let nextPosition = head $ filter (not . wall field) $ allNextCells position
                 in Object t nextPosition position
            2 -> let nextPosition = head $ filter (/= previousPosition) $ filter (not . wall field) $ allNextCells position
                 in Object t nextPosition position
            _ -> move' object
      move' (Object V (r, c) _) =
          let nextPosition = case (r - sr, c - sc) of
                               (dr, dc) | dr > 0 && not (wall field (r - 1, c)) -> (r - 1, c)
                                        | dr < 0 && not (wall field (r + 1, c)) -> (r + 1, c)
                                        | dc > 0 && not (wall field (r, c - 1)) -> (r, c - 1)
                                        | dc < 0 && not (wall field (r, c + 1)) -> (r, c + 1)
                                        | otherwise                             -> head $ filter (not . wall field) $ allNextCells (r, c)
          in Object V nextPosition (r, c)
      move' (Object H (r, c) _) =
          let nextPosition = case (r - sr, c - sc) of
                               (dr, dc) | dc > 0 && not (wall field (r, c - 1)) -> (r, c - 1)
                                        | dc < 0 && not (wall field (r, c + 1)) -> (r, c + 1)
                                        | dr > 0 && not (wall field (r - 1, c)) -> (r - 1, c)
                                        | dr < 0 && not (wall field (r + 1, c)) -> (r + 1, c)
                                        | otherwise                             -> head $ filter (not . wall field) $ allNextCells (r, c)
          in Object H nextPosition (r, c)
      move' (Object L (r, c) (pr, pc)) =
          let nextPosition = head $ filter (not . wall field) $ left (r, c) (pr, pc)
          in Object L nextPosition (r, c)
      move' (Object R (r, c) (pr, pc)) =
          let nextPosition = head $ filter (not . wall field) $ right (r, c) (pr, pc)
          in Object R nextPosition (r, c)
      move' (Object (J l) (r, c) (pr, pc)) =
          let nextPosition = head $ filter (not . wall field) $ (if l then left else right) (r, c) (pr, pc)
          in Object (J (not l)) nextPosition (r, c)
      left (r, c) (pr, pc) = case (r - pr, c - pc) of
                               ( 1,  0) -> [(r, c + 1), (r + 1, c), (r, c - 1)]
                               (-1,  0) -> [(r, c - 1), (r - 1, c), (r, c + 1)]
                               ( 0,  1) -> [(r - 1, c), (r, c + 1), (r + 1, c)]
                               ( 0, -1) -> [(r + 1, c), (r, c - 1), (r - 1, c)]
      right (r, c) (pr, pc) = case (r - pr, c - pc) of
                                ( 1,  0) -> [(r, c - 1), (r + 1, c), (r, c + 1)]
                                (-1,  0) -> [(r, c + 1), (r - 1, c), (r, c - 1)]
                                ( 0,  1) -> [(r + 1, c), (r, c + 1), (r - 1, c)]
                                ( 0, -1) -> [(r - 1, c), (r, c - 1), (r + 1, c)]

candidates :: Position -> [(Position, Char)]
candidates (r, c) =  zip [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1), (r, c)] ['k', 'l', 'j', 'h', '.']

crash :: Step -> Step -> Bool
crash (Step _ pp _) (Step _ p os) = or $ map crash' os
    where
      crash' (Object _ o po) = p == o || po == p && o == pp

allNextCells :: Position -> [Position]
allNextCells (r, c) = [(r + 1, c), (r, c - 1), (r - 1, c), (r, c + 1)]

wall :: Field -> Position -> Bool
wall field position = (field ! position) == Wall

wallCount :: Field -> Position -> Int
wallCount field (r, c) = length $ filter (wall field) [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

dots :: Field -> Int
dots field = length $ filter (== Dot) $ elems field

makeEmpty :: Field -> Position -> Field
makeEmpty field position = field // [(position, Empty)]

load :: String -> (Int, Step)
load contents = let l = lines contents
                    time = read $ head l
                    column = read $ (words (l !! 1)) !! 0
                    row = read $ (words (l !! 1)) !! 1
                    indices = [ (r, c) | r <- [1..row], c <- [1..column] ]
                    input = concat $ drop 2 l
                    cells = map makeCell input
                    Just self = lookup '@' $ zip input indices
                    objects = catMaybes $ map (uncurry makeObject) $ zip input indices
                in (time, Step (listArray ((1, 1), (row, column)) cells) self objects)

makeCell :: Char -> Cell
makeCell '#' = Wall
makeCell '.' = Dot
makeCell _   = Empty

makeObject :: Char -> Position -> Maybe Object
makeObject c position = objectType c >>= \t -> return $ Object t position position

objectType :: Char -> Maybe ObjectType
objectType 'V' = Just V
objectType 'H' = Just H
objectType 'L' = Just L
objectType 'R' = Just R
objectType 'J' = Just (J True)
objectType _   = Nothing

showStep :: Step -> [String]
showStep (Step field position objects) =
    let ((1, 1), (r, c)) = bounds field
        f = (amap cellChar field) // ((position, '@'):map (\(Object t p _) -> (p, objectTypeChar t)) objects)
    in splitEvery c $ elems f

cellChar :: Cell -> Char
cellChar Wall  = '#'
cellChar Empty = ' '
cellChar Dot   = '.'

objectTypeChar :: ObjectType -> Char
objectTypeChar V     = 'V'
objectTypeChar H     = 'H'
objectTypeChar L     = 'L'
objectTypeChar R     = 'R'
objectTypeChar (J _) = 'J'
