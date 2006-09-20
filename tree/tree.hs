module Main (main) where

import System


generateList :: Int -> [[Char]]
generateList n = gen n
 where
     list :: [Char]
     list = take n $ enumFrom 'a'
     gen :: Int -> [[Char]]
     gen n | n == 1    = [ [x] | x <- list ]
           | otherwise = [ x:y | x <- list, y <- gen $ n - 1 ]

data Tree a = Node a [Tree a] deriving Show

makeTree :: [a] -> [Tree a]
makeTree [x]    = [Node x []]
makeTree (x:xs) = map (Node x) $ makeChild xs
 where
     makeChild :: [a] -> [[Tree a]]
     makeChild [y]        = [[Node y []]]
     makeChild yys@(y:ys) = (map (:[]) $ makeTree yys) ++ (map ((Node y []):) $ makeChild ys)

makeTrees :: Int -> [Tree Char]
makeTrees = concatMap makeTree . generateList


type Path a = [Step a]

data Step a = Element a
            | AnyElement

matchPath :: Eq a => Path a -> Tree a -> Bool
matchPath path tree = or $ map (matchSingle path) (candidates tree)
 where
     matchSingle :: Eq a => Path a -> Tree a -> Bool
     matchSingle []          _                = True
     matchSingle [step]      node             = matchStep step node
     matchSingle (step:path) node@(Node x y)  = matchStep step node &&
                                                (or $ map (matchSingle path) y)
     candidates :: Tree a -> [Tree a]
     candidates node@(Node _ c) = node:(concatMap candidates c)

matchStep :: Eq a => Step a -> Tree a -> Bool
matchStep (Element x) (Node y _) = x == y
matchStep AnyElement  (Node _ _) = True

parsePath :: String -> Path Char
parsePath []           = []
parsePath [x]          = [parseStep x]
parsePath (x:'/':path) = (parseStep x):parsePath(path)

parseStep :: Char -> Step Char
parseStep '*' = AnyElement
parseStep x   = Element x

filterTree :: Int -> [String] -> [Tree Char]
filterTree n paths = foldl (flip (filter . matchPath)) (makeTrees n) (map parsePath paths)

main :: IO ()
main = do args <- getArgs
          let n = read $ head args
              paths = tail args
          print $ filterTree n paths

