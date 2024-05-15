module Main where

import Criterion.Main
import Data.Functor.Coyoneda
import Data.Functor.Yoneda
import NList qualified
import SList qualified

list :: [Int]
list = [1 .. 1000000]

slist :: SList.SList Int
slist = SList.from list

nlist :: NList.NList Int
nlist = NList.from list

test, test' :: (Functor f, Show a) => f a -> f Int
test = fmap length . fmap show . fmap length . fmap show
test' = fmap (length . show . length . show)

applyTest :: (Show a) => f a -> Coyoneda f Int
applyTest = test . liftCoyoneda

main :: IO ()
main =
  defaultMain
    [ bgroup
        "normal"
        [ bench "list" $ nf test list,
          bench "slist" $ nf test slist
          -- bench "nlist" $ nf test nlist
        ],
      bgroup
        "yoneda"
        [ bench "slist yoneda" $ nf (lowerYoneda . test . liftYoneda) slist,
          bench "slist compose" $ nf test' slist
          -- bench "nlist yoneda" $ nf (lowerYoneda . test . liftYoneda) nlist
        ],
      bgroup
        "coyoneda"
        [ bench "list" $ nf (lowerCoyoneda . applyTest) list,
          bench "slist" $ nf (lowerCoyoneda . applyTest) slist,
          bench "nlist" $ nf (lowerCoyoneda . hoistCoyoneda NList.to . applyTest) nlist
        ]
    ]
