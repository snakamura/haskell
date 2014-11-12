{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as F
import Control.Monad
import qualified Crypto.Hash as H
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import System.Environment
import System.IO

main :: IO ()
main = liftM head getArgs >>= hashFile >>= print

hashFile :: FilePath -> IO (H.Digest H.SHA1)
hashFile path = withFile path ReadMode $ P.fold H.hashUpdate H.hashInit H.hashFinalize . PB.fromHandle

hashFile' :: FilePath -> IO (H.Digest H.SHA1)
hashFile' path = withFile path ReadMode $ hashProducer . PB.fromHandle

hashProducer :: (Monad m, H.HashAlgorithm a) => P.Producer B.ByteString m () -> m (H.Digest a)
hashProducer = F.purely P.fold hash

hashVector :: V.Vector B.ByteString -> H.Digest H.SHA1
hashVector = F.fold hash

hash :: H.HashAlgorithm a => F.Fold B.ByteString (H.Digest a)
hash = F.Fold H.hashUpdate H.hashInit H.hashFinalize
