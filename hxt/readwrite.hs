module Main (main) where

import System.Environment
import Text.XML.HXT.Arrow

main :: IO ()
main = do [src, dst] <- getArgs
          runX (readDocument [(a_validate, v_0)] src >>>
                processChildren (eelem "foo") >>>
                writeDocument [] dst)
          return ()

