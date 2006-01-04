module FileStore (FileStore(),
                  newFileStore)
    where

import           Control.Monad
import           Data.List
import qualified System.Directory as Dir

import qualified Store


newtype FileStore = FS String

newFileStore :: String -> FileStore
newFileStore dir = FS dir

instance Store.Store FileStore where
    existPage fs page = Dir.doesFileExist $ getPagePath fs page
    
    getPage fs page = catch (readFile $ getPagePath fs page)
                            (\ e -> return "")
    
    updatePage fs page body = writeFile (getPagePath fs page) body
    
    removePage fs page = catch (Dir.removeFile $ getPagePath fs page)
                               (\ e -> return ())
    
    listPages fs@(FS dir) = Dir.getDirectoryContents dir >>=
                            filterM isPage >>=
                            return . sort >>=
                            mapM createPageMetadata
        where
            isPage :: String -> IO Bool
            isPage (c:_)  = return $ c /= '.'
            createPageMetadata :: String -> IO Store.PageMetadata
            createPageMetadata page = (Dir.getModificationTime $ getPagePath fs page) >>=
                                      return . Store.PageMetadata page

getPagePath :: FileStore -> String -> FilePath
getPagePath (FS dir) page = dir ++ page
