import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "Hello World"
     clearColor $= Color4 0.0 0.0 1.0 0.0
     displayCallback $= display
     mainLoop

display =
  do clear [ColorBuffer]
     flush
