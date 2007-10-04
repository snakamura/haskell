{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (liftM)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    words <- liftM lines getContents
    print $ count $ genEdges words

--count :: Ord a => [(a, a)] -> Int
count = Set.size . merge{-Edges-}

--mergeEdges :: Ord a => [(a, a)] -> Set (Set a)
mergeEdges = merge . Set.fromList . map (\ (x, y) -> Set.fromList [x, y])

--merge :: Ord a => Set (Set a) -> Set (Set a)
merge = Set.fold merge' Set.empty
 where
     merge' h t | Set.null t = Set.singleton h
                | otherwise  = let (th, tt) = Set.deleteFindMin t
                               in if not $ Set.null $ Set.intersection h th
                                      then merge' (Set.union h th) tt
                                      else Set.insert th $ merge' h tt

hasLink x y = hasLink' (sort x) (sort y)
 where
     hasLink' xss@(x:xs) yss@(y:ys) | x == y    = hasLink' xs ys
                                    | otherwise = hasLinkL xs yss ||
                                                  xs == ys ||
                                                  hasLinkR xss ys
     hasLink' []         _                      = True
     hasLinkL xss@(x:xs) (y:ys) | x == y    = hasLinkL xs ys
                                | otherwise = xss == ys
     hasLinkL []         [_]                = True
     hasLinkR = flip hasLinkL

genEdges (word:words) =
    let l = [ [word, w] | w <- words, hasLink word w ]
        s = if null l
                then Set.singleton $ Set.singleton word
                else Set.fromList $ map Set.fromList l
    in Set.union s $ genEdges words
genEdges [] = Set.empty
