module Config (Config,
               loadConfig,
               getConfig)
    where

import qualified TextUtil


type Config = [(String, String)]

loadConfig :: FilePath -> IO Config
loadConfig path = readFile path >>= return . parseConfig

getConfig :: Config -> String -> String
getConfig config name = case lookup name config of
                             Just v  -> v
                             Nothing -> ""

parseConfig :: String -> Config
parseConfig = concatMap parseLine . lines
    where
        parseLine :: String -> [(String, String)]
        parseLine [] = []
        parseLine ('#':_) = []
        parseLine line = case break ('=' ==) line of
                              ([], _) -> []
                              (_, []) -> []
                              (k, _:v) -> [(TextUtil.trimString k, TextUtil.trimString v)]
