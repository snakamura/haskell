import Control.Monad
import Data.Array.Storable
import qualified Data.ByteString as BS
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLUT
import System.IO

import qualified Matrix as M

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     texture Texture2D $= Enabled
     program <- join $ liftM2 createProgram (createShader vertexShaderSource) 
                                            (createShader fragmentShaderSource)
     texture <- createTexture "texture.rgb" 256 256
     mvpLoc <- get $ uniformLocation program "u_mvp"
     textureLoc <- get $ uniformLocation program "u_texture"
     positionLoc <- get $ attribLocation program "a_position"
     texCoordLoc <- get $ attribLocation program "a_texCoord"
     buffer <- createBuffer ([ 0.9,  0.9, 1.0, 0.0,
                               0.9, -0.9, 1.0, 1.0,
                              -0.9, -0.9, 0.0, 1.0,
                              -0.9,  0.9, 0.0, 0.0] :: [GLfloat])
     sizeRef <- newIORef $ Size 0 0
     displayCallback $= display program mvpLoc textureLoc positionLoc texCoordLoc buffer texture sizeRef
     reshapeCallback $= Just (reshape sizeRef)
     mainLoop

display program mvpLoc textureLoc positionLoc texCoordLoc buffer texture sizeRef =
  do clear [ColorBuffer]
     currentProgram $= Just program
     bindBuffer ArrayBuffer $= Just buffer
     modelViewMatrix <- M.identity
--     M.translate modelViewMatrix 0.0 0.5 0.0
--     M.rotate modelViewMatrix 30 0.0 0.0 1.0
--     M.scale modelViewMatrix 0.5 0.5 0.5
     projectionMatrix <- M.identity
     Size w h <- readIORef sizeRef
--     M.ortho projectionMatrix (negate $ fromIntegral w/500) 
--                              (fromIntegral w/500) 
--                              (negate $ fromIntegral h/500) 
--                              (fromIntegral h/500) 
--                              (-1.0) 
--                              1.0
     M.perspective projectionMatrix 90 (fromIntegral w/fromIntegral h) (1.0) 10.0
--     M.frustum projectionMatrix (-1.0) 1.0 (-1.0) 1.0 1.0 5.0
     M.translate projectionMatrix 0.0 0.0 (-2.0)
     M.rotate projectionMatrix (-60) 1.0 0.0 0.0
     mvpMatrix <- M.identity
     M.multiply mvpMatrix projectionMatrix
     M.multiply mvpMatrix modelViewMatrix
     M.withMatrix mvpMatrix $ \ptr ->
       glUniformMatrix4fv (extractUniformLoc mvpLoc) 1 0 ptr
     textureBinding Texture2D $= Just texture
     textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
     uniform textureLoc $= TexCoord1 (0 :: GLuint)
     vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float (toEnum (4*sizeOf(0 :: GLfloat))) (intPtrToPtr 0))
     vertexAttribArray positionLoc $= Enabled
     vertexAttribPointer texCoordLoc $= (ToFloat, VertexArrayDescriptor 2 Float (toEnum (4*sizeOf(0 :: GLfloat))) (intPtrToPtr (toEnum (2*sizeOf(0 :: GLfloat)))))
     vertexAttribArray texCoordLoc $= Enabled
     drawArrays TriangleFan 0 4
     flush

reshape sizeRef size@(Size w h) =
  do viewport $= (Position 0 0, size)
     writeIORef sizeRef size

vertexShaderSource = "uniform mat4 u_mvp;               \
                     \attribute vec4 a_position;        \
                     \attribute vec2 a_texCoord;        \
                     \varying vec2 v_texCoord;          \
                     \void main() {                     \
                     \  gl_Position = u_mvp*a_position; \
                     \  v_texCoord = a_texCoord;        \
                     \}"

fragmentShaderSource = "uniform sampler2D u_texture;                       \
                       \varying vec2 v_texCoord;                           \
                       \void main() {                                      \
                       \  gl_FragColor = texture2D(u_texture, v_texCoord); \
                       \}"

createShader source =
  do [shader] <- genObjectNames 1
     shaderSource shader $= [source]
     compileShader shader
     return shader

createProgram vertexShader fragmentShader =
  do [program] <- genObjectNames 1
     attachedShaders program $= ([vertexShader], [fragmentShader])
     linkProgram program
     return program

createBuffer l =
  do [buffer] <- genObjectNames 1
     bindBuffer ArrayBuffer $= Just buffer
     arr <- newListArray (0, length l - 1) l
     withStorableArray arr $ \ptr ->
       bufferData ArrayBuffer $= (toEnum (length l*sizeOf(head l)), ptr, StaticDraw)
     bindBuffer ArrayBuffer $= Nothing
     return buffer

createTexture path width height =
  do [texture] <- genObjectNames 1
     textureBinding Texture2D $= Just texture
     rowAlignment Unpack $= 1
     image <- BS.readFile path
     BS.useAsCString image $ \ptr ->
       texImage2D Nothing NoProxy 0 RGB' (TextureSize2D width height) 0 (PixelData RGB UnsignedByte ptr)
     textureBinding Texture2D $= Nothing
     return texture

extractUniformLoc loc = read $ last $ words $ show loc
