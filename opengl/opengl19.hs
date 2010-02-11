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
     initialDisplayMode $= [RGBAMode, DoubleBuffered]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     texture Texture2D $= Enabled
--     blend $= Enabled
--     blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--     polygonSmooth $= Enabled
--     hint PolygonSmooth $= Nicest
     program <- join $ liftM2 createProgram (createShader vertexShaderSource) 
                                            (createShader fragmentShaderSource)
     texture <- createTexture "texture.rgb" 256 256
     counterLoc <- get $ uniformLocation program "u_counter"
     mvpLoc <- get $ uniformLocation program "u_mvp"
     textureLoc <- get $ uniformLocation program "u_texture"
     positionLoc <- get $ attribLocation program "a_position"
     let vertices :: [GLfloat]
         vertices = concat $ do let n = 32
                                    w = 4
                                x <- [0 .. n - 1]
                                y <- [0 .. n - 1]
                                let l = -1.0 + 2*x/n
                                    t = 1.0 - 2*y/n
                                    r = -1.0 + 2*(x + 1)/n
                                    b = 1.0 - 2*(y + 1)/n
                                return [l, t,
                                        l, b,
                                        r, t,
                                        r, t,
                                        l, b,
                                        r, b]
     buffer <- createBuffer vertices
     sizeRef <- newIORef $ Size 0 0
     counterRef <- newIORef 0
     displayCallback $= display (fromIntegral (length vertices `div` 2)) program counterLoc mvpLoc textureLoc positionLoc buffer texture sizeRef counterRef
     reshapeCallback $= Just (reshape sizeRef)
     timer counterRef
     mainLoop

display count program counterLoc mvpLoc textureLoc positionLoc buffer texture sizeRef counterRef =
  do clear [ColorBuffer]
     currentProgram $= Just program
     counter <- readIORef counterRef
     bindBuffer ArrayBuffer $= Just buffer
--     uniform counterLoc $= TexCoord1 (counter :: GLint)
     glUniform1f (extractUniformLoc counterLoc) counter
     modelViewMatrix <- M.identity
     M.rotate modelViewMatrix 20 0.0 1.0 0.0
     M.scale modelViewMatrix 1.0 1.0 0.05
     projectionMatrix <- M.identity
     Size w h <- readIORef sizeRef
     M.perspective projectionMatrix 70 (fromIntegral w/fromIntegral h) (1.0) 10.0
     M.translate projectionMatrix 0.0 0.0 (-2.0)
     mvpMatrix <- M.identity
     M.multiply mvpMatrix projectionMatrix
     M.multiply mvpMatrix modelViewMatrix
     M.withMatrix mvpMatrix $ \ptr ->
       glUniformMatrix4fv (extractUniformLoc mvpLoc) 1 0 ptr
     textureBinding Texture2D $= Just texture
     textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
     vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (intPtrToPtr 0))
     vertexAttribArray positionLoc $= Enabled
     drawArrays Triangles 0 count
     swapBuffers

reshape sizeRef size@(Size w h) =
  do viewport $= (Position 0 0, size)
     writeIORef sizeRef size

timer counterRef =
  do modifyIORef counterRef (+1)
     postRedisplay Nothing
     addTimerCallback 60 $ timer counterRef

vertexShaderSource = "const float PI = 3.1415926535;                                          \
                     \uniform float u_counter;                                                \
                     \uniform mat4 u_mvp;                                                     \
                     \attribute vec4 a_position;                                              \
                     \varying vec2 v_texCoord;                                                \
                     \void main() {                                                           \
                     \  vec4 pos = a_position;                                                \
                     \  pos.z = sin((pos.x - mod(u_counter, 128.0)/20.0)*PI*4)*(pos.x + 1.0); \
                     \  gl_Position = u_mvp*pos;                                              \
                     \  v_texCoord = vec2((pos.x + 1.0)/2, (1.0 - pos.y)/2);                  \
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
