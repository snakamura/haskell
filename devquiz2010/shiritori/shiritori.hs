import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

data Step = Step !String [Step] !Bool deriving Show

main :: IO ()
main = do args <- getArgs
          words <- liftM lines $ readFile $ head args
          let startWord = head words
              otherWords = tail words
              wordsMap = Map.fromList $ map (head . head &&& id)
                                      $ groupBy ((==) `on` head)
                                      $ sort otherWords
              step = buildStep startWord wordsMap False
          next step

next :: Step -> IO ()
next step@(Step _ nextSteps _) =
    do putStrLn $ unwords $ map (\(Step w _ _) -> w) nextSteps
       word <- getLine
       case find (\(Step w _ _) -> w == word) nextSteps of
         Just step -> next step
         Nothing -> do putStrLn "error, retry"
                       next step

buildStep :: String -> Map Char [String] -> Bool -> Step
buildStep start words myTurn =
    let nextWords = candidates start words
        nextSteps = map (\word -> buildStep word (Map.update (Just . delete word) (head word) words) (not myTurn)) nextWords
        filteredNextSteps = if not myTurn then
                                take 1 $ filter (not . mustLose) nextSteps
                            else
                                nextSteps
    in Step start filteredNextSteps (mustLose' myTurn filteredNextSteps)

mustLose :: Step -> Bool
mustLose (Step _ _ lose) = lose

mustLose' :: Bool -> [Step] -> Bool
mustLose' myTurn nextSteps =
    if myTurn then
        not (null nextSteps) && or' (map mustLose nextSteps)
    else
        null nextSteps || and' (map mustLose nextSteps)

candidates :: String -> Map Char [String] -> [String]
candidates previous words = fromMaybe [] $ Map.lookup (last previous) words

and' = foldl' (&&) True
or' = foldl' (||) False
