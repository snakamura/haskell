import Control.Monad
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     program <- join $ liftM2 createProgram (createShader vertexShaderSource) 
                                            (createShader fragmentShaderSource)
     positionLoc <- get $ attribLocation program "a_position"
     colorLoc <- get $ attribLocation program "a_color"
     buffer <- createBuffer ([ 0.0,  0.9, 1.0, 0.0, 0.0,
                              -0.9, -0.9, 0.0, 1.0, 0.0,
                               0.9, -0.9, 0.0, 0.0, 1.0] :: [GLfloat])
     displayCallback $= display program positionLoc colorLoc buffer
     mainLoop

display program positionLoc colorLoc buffer =
  do clear [ColorBuffer]
     currentProgram $= Just program
     bindBuffer ArrayBuffer $= Just buffer
     vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float (toEnum (5*sizeOf(0 :: GLfloat))) (intPtrToPtr 0))
     vertexAttribArray positionLoc $= Enabled
     vertexAttribPointer colorLoc $= (ToFloat, VertexArrayDescriptor 3 Float (toEnum (5*sizeOf(0 :: GLfloat))) (intPtrToPtr (toEnum (2*sizeOf(0 :: GLfloat)))))
     vertexAttribArray colorLoc $= Enabled
     drawArrays Triangles 0 6
     flush

vertexShaderSource = "attribute vec4 a_position;  \
                     \attribute vec4 a_color;     \
                     \varying vec4 v_color;       \
                     \void main() {               \
                     \  gl_Position = a_position; \
                     \  v_color = a_color;        \
                     \}"

fragmentShaderSource = "varying vec4 v_color;     \
                       \void main() {             \
                       \  gl_FragColor = v_color; \
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
