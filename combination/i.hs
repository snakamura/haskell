import Data.Bifunctor
import Data.List

combination :: Ord a => [a] -> Int -> [[[a]]]
combination items 1 = [[items]]
combination items 2 = concatMap (map toList . pickN (sort items)) [1 .. length items - 1]
  where
    toList (x, y) = [x, y]
combination items n = concatMap splitLast $ combination items (n - 1)
  where
    splitLast items = map ((init items) ++) $ combination (last items) 2

pickN :: Ord a => [a] -> Int -> [([a], [a])]
pickN items 0 = [([], items)]
pickN items n = nub $ concatMap pickNext $ pickN items (n - 1)
  where
    pickNext (items, restItems) = map (first (sort . (: items))) $ pick restItems

pick :: Eq a => [a] -> [(a, [a])]
pick items = map (\item -> (item, delete item items)) items
