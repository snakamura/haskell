import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "Window"
     clearColor $= Color4 0.0 0.0 1.0 0.0
     displayCallback $= display
     mainLoop

display =
  do clear [ColorBuffer]
     color $ (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
     let points :: [(GLfloat, GLfloat)]
         points = [(0.9, 0.9), (0.9, -0.9), (-0.9, -0.9), (-0.9, 0.9)]
     renderPrimitive LineLoop $
       mapM_ (vertex . uncurry Vertex2) points
     flush
