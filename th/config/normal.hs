import Control.Monad
import Data.Maybe

data Config = Config {
  name  :: !String,
  value :: !Int
} deriving Show

main = do config <- liftM parse $ readFile "config"
          print config

parse :: String -> Config
parse s = let values = catMaybes $ map parseLine $ lines s
          in Config (fromMaybe "" $ lookup "name" values)
                    (fromMaybe 0 $ lookup "value" values >>= maybeRead)

parseLine :: String -> Maybe (String, String)
parseLine ""      = Nothing
parseLine ('#':_) = Nothing
parseLine s       = case break (== '=') s of
                      ("", _) -> Nothing
                      (n, v)  -> Just (n, tail v)

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(v, [])] -> Just v
                _         -> Nothing
