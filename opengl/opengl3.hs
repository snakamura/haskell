import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "First Window"
     clearColor $= Color4 0.0 0.0 1.0 0.0
     displayCallback $= display
     createWindow "Second Window"
     clearColor $= Color4 1.0 0.0 0.0 0.0
     displayCallback $= display
     actionOnWindowClose $= ContinueExectuion
     mainLoop

display =
  do clear [ColorBuffer]
     flush
