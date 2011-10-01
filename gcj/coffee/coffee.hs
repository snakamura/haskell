{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State (MonadState, evalState, get, put)
import Data.Hashable (Hashable(..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (delete, foldl', maximumBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PQ
import Text.Printf (printf)


type Count = Int
type Expiration = Integer
type Satisfaction = Integer
type Coffee = (Count, Expiration, Satisfaction)


main :: IO ()
main = interact (writeOutput . map solve . readInput)


readInput :: String -> [([Coffee], Integer)]
readInput = readItem . tail . lines
    where
      readItem [] = []
      readItem (h:c) = let [n, k] = words h
                           coffee = map readCoffee $ take (read n) c
                       in (coffee, read k):readItem (drop (read n) c)
      readCoffee l = let [c, e, s] = words l
                     in (read c, read e, read s)


writeOutput :: [Integer] -> String
writeOutput = unlines . zipWith (printf "Case #%d: %d") [1 :: Int ..]


solve :: ([Coffee], Integer) -> Integer
solve = uncurry maxSatisfaction


data Item = Item Integer [Coffee] Satisfaction Integer

makeItem :: Integer -> [Coffee] -> Satisfaction -> Item
makeItem day coffee s = Item day coffee s $ s + sum (map (\(_, _, s) -> s) coffee)

instance Eq Item where
    Item _ _ _ p1 == Item _ _ _ p2 = p1 == p2

instance Ord Item where
    compare (Item _ _ _ p1) (Item _ _ _ p2) = compare p1 p2

--instance Hashable Item where
--    hashWithSalt salt (Item d c s _) = salt `hashWithSalt` d `hashWithSalt` c `hashWithSalt` s

data OpenItems = OpenItems (MaxQueue Item) (HashMap [Coffee] Item)

data ClosedItems = ClosedItems (HashMap [Coffee] Item)

emptyOpenItems :: OpenItems
emptyOpenItems = OpenItems PQ.empty HashMap.empty

getOpenItem :: [Coffee] -> OpenItems -> Maybe Item
getOpenItem coffee (OpenItems _ m) = HashMap.lookup coffee m

addOpenItem :: Item -> OpenItems -> OpenItems
addOpenItem item@(Item _ c _ _) (OpenItems q m) = OpenItems (PQ.insert item q) (HashMap.insert c item m)

removeOpenItem :: Item -> OpenItems -> OpenItems
removeOpenItem item@(Item _ c _ _) (OpenItems q m) = OpenItems q (HashMap.delete c m)

emptyClosedItems :: ClosedItems
emptyClosedItems = ClosedItems HashMap.empty

getClosedItem :: [Coffee] -> ClosedItems -> Maybe Item
getClosedItem coffee (ClosedItems m) = HashMap.lookup coffee m

removeClosedItem :: Item -> ClosedItems -> ClosedItems
removeClosedItem item@(Item _ coffee _ _) (ClosedItems m) = ClosedItems $ HashMap.delete coffee m

getNextOpenItem :: OpenItems -> ClosedItems -> Maybe (Item, OpenItems, ClosedItems)
getNextOpenItem (OpenItems q os) (ClosedItems cs) =
    case PQ.getMax q of
      Just item@(Item _ coffee _ _) | isJust $ HashMap.lookup coffee os ->
                                        Just (item, OpenItems (PQ.deleteMax q) (HashMap.delete coffee os), ClosedItems (HashMap.insert coffee item cs))
                                    | otherwise                       ->
                                        getNextOpenItem (OpenItems (PQ.deleteMax q) os) (ClosedItems cs)
      Nothing -> Nothing

maxSatisfaction :: [Coffee] -> Integer -> Satisfaction
maxSatisfaction coffee days =
    evalState (maxSatisfaction' days 0) (addOpenItem (makeItem 0 coffee 0) emptyOpenItems, emptyClosedItems)


maxSatisfaction' :: MonadState (OpenItems, ClosedItems) m => Integer -> Satisfaction -> m Satisfaction
maxSatisfaction' days lastSatisfaction =
    do (openItems, closedItems) <- get
       case getNextOpenItem openItems closedItems of
         Just (Item day coffee satisfaction _, nextOpenItems, nextClosedItems) ->
             if day > days then
--                 return $ max satisfaction lastSatisfaction
--                 return satisfaction
                 return lastSatisfaction
             else
                 do let nextItems = map (\(s, c) -> makeItem (day + 1) c s) $ drink (satisfaction, coffee)
                    put $ foldr insert (nextOpenItems, nextClosedItems) nextItems
                    maxSatisfaction' days $ max satisfaction lastSatisfaction
         Nothing -> return lastSatisfaction
    where
      insert item@(Item _ coffee _ p) (openItems, closedItems) =
          case (getClosedItem coffee closedItems, getOpenItem coffee openItems) of
            (Nothing, Nothing) -> (addOpenItem item openItems, closedItems)
            (Just closedItem@(Item _ _ _ cp), _) | p > cp -> (addOpenItem item openItems, removeClosedItem closedItem closedItems)
            (_, Just openItem@(Item _ _ _ op)) | p > op -> (addOpenItem item $ removeOpenItem openItem openItems, closedItems)
            (_, _) -> (openItems, closedItems)


_maxSatisfaction :: [Coffee] -> Integer -> Satisfaction
_maxSatisfaction coffee days = fst $ maximumBy (comparing fst) $ foldl' f [(0, coffee)] [1 .. days]
    where
      f d _ = concatMap drink d


drink :: (Satisfaction, [Coffee]) -> [(Satisfaction, [Coffee])]
drink (ss, []) = [(ss, [])]
drink (ss, cs) = map d cs
    where
      d coffee@(c, e, s) = (ss + s, filter f $ (c - 1, e - 1, s):map x (delete coffee cs))
      x (c, e, s) = (c, e - 1, s)
      f (0, _, _) = False
      f (_, 0, _) = False
      f _         = True
