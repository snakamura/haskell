{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Text.Regex.Posix

import Regex

regex = [$qr|ab*cd|]

main = do putStrLn $ show (match regex "abbbbcd" :: Bool)
