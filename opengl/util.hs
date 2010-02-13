module Util (
  loadShader,
  createProgram,
  createArrayBuffer,
  createTexture,
  extractUniformLoc
) where

import Control.Monad
import Data.Array.Storable
import qualified Data.ByteString as BS
import Foreign.Storable
import Graphics.Rendering.OpenGL
import System.IO

loadShader :: Shader s => String -> IO s
loadShader path =
  do source <- readFile path
     [shader] <- genObjectNames 1
     shaderSource shader $= [source]
     compileShader shader
     unlessM (get $ compileStatus shader) $
       get (shaderInfoLog shader) >>= hPutStrLn stderr
     return shader

createProgram :: VertexShader -> FragmentShader -> IO Program
createProgram vertexShader fragmentShader =
  do [program] <- genObjectNames 1
     attachedShaders program $= ([vertexShader], [fragmentShader])
     linkProgram program
     unlessM (get $ linkStatus program) $
       get (programInfoLog program) >>= hPutStrLn stderr
     return program

createArrayBuffer :: Storable a => [a] -> IO BufferObject
createArrayBuffer l =
  do [buffer] <- genObjectNames 1
     bindBuffer ArrayBuffer $= Just buffer
     arr <- newListArray (0, length l - 1) l
     withStorableArray arr $ \ptr ->
       bufferData ArrayBuffer $= (toEnum (length l*sizeOf(head l)), ptr, StaticDraw)
     bindBuffer ArrayBuffer $= Nothing
     return buffer

createTexture :: String -> GLsizei -> GLsizei -> IO TextureObject
createTexture path width height =
  do [texture] <- genObjectNames 1
     textureBinding Texture2D $= Just texture
     rowAlignment Unpack $= 1
     image <- BS.readFile path
     BS.useAsCString image $ \ptr ->
       texImage2D Nothing NoProxy 0 RGB' (TextureSize2D width height) 0 (PixelData RGB UnsignedByte ptr)
     textureBinding Texture2D $= Nothing
     return texture

extractUniformLoc :: UniformLocation -> GLint
extractUniformLoc loc = read $ last $ words $ show loc


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = p >>= flip unless s
