import Control.Monad
import Data.Array.Storable
import qualified Data.ByteString as BS
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     texture Texture2D $= Enabled
     program <- join $ liftM2 createProgram (createShader vertexShaderSource) 
                                            (createShader fragmentShaderSource)
     texture <- createTexture "texture.rgb" 256 256
     positionLoc <- get $ attribLocation program "a_position"
     textureLoc <- get $ attribLocation program "a_texCoord"
     buffer <- createBuffer ([ 0.9,  0.9, 1.0, 0.0,
                               0.9, -0.9, 1.0, 1.0,
                              -0.9, -0.9, 0.0, 1.0,
                              -0.9,  0.9, 0.0, 0.0] :: [GLfloat])
     displayCallback $= display program positionLoc textureLoc buffer texture
     mainLoop

display program positionLoc textureLoc buffer texture =
  do clear [ColorBuffer]
     currentProgram $= Just program
     bindBuffer ArrayBuffer $= Just buffer
     textureBinding Texture2D $= Just texture
     textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
     vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float (toEnum (4*sizeOf(0 :: GLfloat))) (intPtrToPtr 0))
     vertexAttribArray positionLoc $= Enabled
     vertexAttribPointer textureLoc $= (ToFloat, VertexArrayDescriptor 2 Float (toEnum (4*sizeOf(0 :: GLfloat))) (intPtrToPtr (toEnum (2*sizeOf(0 :: GLfloat)))))
     vertexAttribArray textureLoc $= Enabled
     drawArrays TriangleFan 0 4
     flush

vertexShaderSource = "attribute vec4 a_position;  \
                     \attribute vec2 a_texCoord;  \
                     \varying vec2 v_texCoord;    \
                     \void main() {               \
                     \  gl_Position = a_position; \
                     \  v_texCoord = a_texCoord;  \
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
