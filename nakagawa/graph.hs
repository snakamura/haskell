{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Set (Set)
import qualified Data.Set as Set

--count :: Ord a => [(a, a)] -> Int
count = Set.size . mergeEdges

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
