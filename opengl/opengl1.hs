import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "Hello World"
     displayCallback $= clear [ColorBuffer]
     mainLoop
