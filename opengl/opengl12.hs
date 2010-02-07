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
     loc <- get $ attribLocation program "a_position"
     positions <- newListArray (0, 5) ([0.0, 0.9, -0.9, -0.9, 0.9, -0.9] :: [GLfloat])
     withStorableArray positions $ \ptr ->
       do vertexAttribPointer loc $= (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
          vertexAttribArray loc $= Enabled
          drawArrays Triangles 0 6
     flush

vertexShaderSource = "attribute vec4 a_position;  \
                     \void main() {               \
                     \  gl_Position = a_position; \
                     \}"

fragmentShaderSource = "void main() {                              \
                       \  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); \
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
