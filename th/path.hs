{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Functor
import Language.Haskell.TH
import System.Directory
import System.FilePath

main = print filePath

filePath :: String
--filePath = $(do
--    dir <- runIO getCurrentDirectory
--    filename <- loc_filename <$> location
--    litE $ stringL $ dir </> filename)
filePath = $(liftM2 (</>) (runIO getCurrentDirectory) (loc_filename <$> location) >>= litE . stringL)
