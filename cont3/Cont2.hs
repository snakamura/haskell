module Cont2 where

import Cont
import Prelude hiding (product)

product :: [Int] -> Int
product [] = 1
product (x : xs) = x * product xs

productCPS :: [Int] -> (Int -> r) -> r
productCPS [] next = next 1
productCPS (x : xs) next = productCPS xs $ \p -> next (x * p)

productCont :: [Int] -> Cont r Int
productCont xs = do
  case xs of
    [] -> pure 1
    (x : xs') -> do
      p <- productCont xs'
      pure $ x * p

product' :: [Int] -> Int
product' [] = 1
product' (0 : _) = 0
product' (x : xs) = x * product' xs

productCPS' :: [Int] -> (Int -> r) -> r
productCPS' [] next = next 1
productCPS' (0 : _) next = next 0
productCPS' (x : xs) next = productCPS' xs $ \p -> next (x * p)

productCPS'' :: [Int] -> (Int -> r) -> r
productCPS'' [] next = next 1
productCPS'' xs next =
  let exit = next
      go [] next' = next' 1
      go (0 : _) next' = exit 0
      go (x : xs) next' = go xs $ \p -> next' (x * p)
   in go xs next

productCont' :: [Int] -> Cont r Int
productCont' xs =
  case xs of
    [] -> pure 1
    (0 : _) -> pure 0
    (x : xs') -> do
      p <- productCont' xs'
      pure $ x * p

productCont'' :: [Int] -> Cont r Int
productCont'' xs = callCC $ \exit ->
    let go [] = pure 1
        go (0 : _) = exit 0
        go (x : xs') = do
          p <- go xs'
          pure $ x * p
     in go xs
