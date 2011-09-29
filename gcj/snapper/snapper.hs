module Main where

data State = ON | OFF deriving Show

data Power = P | N deriving Show

data Snapper = Snapper State Power deriving Show


main :: IO ()
main = interact $ writeOutput . zip [1..] . map (uncurry solve2) . readInput


readInput :: String -> [(Int, Int)]
readInput = map r . tail . lines
    where
      r s = let [n, k] = words s
            in (read n, read k)


writeOutput :: [(Int, Bool)] -> String
writeOutput = unlines . map (uncurry write)
    where
      write n r = "Case #" ++ show n ++ ": " ++ if r then "ON" else "OFF"


solve :: Int -> Int -> Bool
solve n k = state $ last $ head $ drop k $ iterate nextSnappers $ initialSnappers n
    where
      state (Snapper ON P) = True
      state _              = False


initialSnappers :: Int -> [Snapper]
initialSnappers n = take n $ Snapper OFF P:repeat (Snapper OFF N)


nextSnappers :: [Snapper] -> [Snapper]
nextSnappers snappers = go P snappers
    where
      go _     []               = []
      go power (Snapper s p:ss) = let nextState = toggleState s p
                                  in Snapper nextState power:go (nextPower nextState power) ss


toggleState :: State -> Power -> State
toggleState ON  P = OFF
toggleState OFF P = ON
toggleState s   N = s


nextPower :: State -> Power -> Power
nextPower ON P = P
nextPower _  _ = N


solve2 :: Int -> Int -> Bool
solve2 n k = (k + 1) `mod` 2 ^ n == 0
