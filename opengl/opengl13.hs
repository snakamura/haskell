import Control.Monad
import Data.Array.Storable
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     program <- join $ liftM2 createProgram (createShader vertexShaderSource) 
                                            (createShader fragmentShaderSource)
     displayCallback $= display program
     mainLoop

display program =
  do clear [ColorBuffer]
     currentProgram $= Just program
     positionLoc <- get $ attribLocation program "a_position"
     colorLoc <- get $ attribLocation program "a_color"
     positions <- newListArray (0, 5) ([0.0, 0.9, -0.9, -0.9, 0.9, -0.9] :: [GLfloat])
     colors <- newListArray (0, 9) ([1, 0, 0, 0, 1, 0, 0, 0, 1] :: [GLfloat])
     withStorableArray positions $ \positionsPtr ->
       withStorableArray colors $ \colorsPtr ->
         do vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 positionsPtr)
            vertexAttribArray positionLoc $= Enabled
            vertexAttribPointer colorLoc $= (ToFloat, VertexArrayDescriptor 3 Float 0 colorsPtr)
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
