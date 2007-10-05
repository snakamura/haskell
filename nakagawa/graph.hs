{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (liftM)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

main = do
    words <- liftM lines getContents
    print $ Set.size $ merge $ gen $ map sort words

merge = Set.fold merge' Set.empty
 where
     merge' h t | Set.null t = Set.singleton h
                | otherwise  = let (th, tt) = Set.deleteFindMin t
                               in if not $ Set.null $ Set.intersection h th
                                      then merge' (Set.union h th) tt
                                      else Set.insert th $ merge' h tt

hasLink []         _                      = True
hasLink xss@(x:xs) yss@(y:ys) | x == y    = hasLink xs ys
                              | otherwise = hasLinkL xs yss || xs == ys || hasLinkR xss ys
 where
     hasLinkL xss@(x:xs) (y:ys) | x == y    = hasLinkL xs ys
                                | otherwise = xss == ys
     hasLinkL []         [_]                = True
     hasLinkR = flip hasLinkL

gen (word:words) = let s = Set.fromList $ word:[ w | w <- words, hasLink word w ]
                   in Set.insert s $ gen words
gen [] = Set.empty
