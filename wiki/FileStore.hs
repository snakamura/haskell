module FileStore (newFileStore) where

import Control.Monad
import Data.List
import System.Directory

import Store

newtype FileStore = FS String

newFileStore :: String -> FileStore
newFileStore dir = FS dir

instance Store FileStore where
    existPage fs page = doesFileExist $ getPagePath fs page
    
    getPage fs page = catch (readFile $ getPagePath fs page)
                            (\ e -> return "")
    
    updatePage fs page body = writeFile (getPagePath fs page) body
    
    removePage fs page = removeFile $ getPagePath fs page
    
    listPages fs@(FS dir) = getDirectoryContents dir >>=
                            filterM isFile >>=
                            return . sort >>=
                            mapM pairFileTime
        where
            isFile :: String -> IO Bool
            isFile (c:_)  = return $ c /= '.'
            pairFileTime :: String -> IO PageMetadata
            pairFileTime file = (getModificationTime $ getPagePath fs file) >>=
                                return . PM file

getPagePath :: FileStore -> String -> FilePath
getPagePath (FS dir) page = dir ++ page
