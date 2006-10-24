module Main (main) where

import Control.Monad
import System
import Text.ParserCombinators.Parsec
import Text.XML.HXT.Arrow

main :: IO ()
main = do args <- getArgs
          let name = (read $ args !! 0) - 1
              address = (read $ args !! 1) - 1
          addresses <- (map (parseAddress  name address) . tail . lines) `liftM` getContents
          runX (xml addresses >>> putXmlDocument "-")
          return ()

parseAddress :: Int -> Int -> String -> Address
parseAddress name address s =
    case runParser csvParser () "" s of
        Right columns -> Address (columns !! name) (columns !! address)
        Left err      -> error $ show err

data Address = Address {
    name    :: String,
    address :: String
} deriving Show

csvParser :: Parser [String]
csvParser = sepBy value (char ',')
 where
     value :: Parser String
     value = do char '"'
                v <- many (noneOf "\"")
                char '"'
                return v
         <|> many (noneOf ",")

xml = selem "addressBook" . map entry
 where
     entry a = selem "entry" [
                   selem "name" [txt $ name a],
                   selem "addresses" [
                       selem "address" [txt $ address a]
                   ]
               ]
