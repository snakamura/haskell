import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     displayCallback $= display
     mainLoop

display =
  do clear [ColorBuffer]
     let points :: [(Color3 GLfloat, (GLfloat, GLfloat))]
         points = [(Color3 1.0 0.0 0.0, ( 0.9,  0.9)),
                   (Color3 0.0 1.0 0.0, ( 0.9, -0.9)), 
                   (Color3 0.0 0.0 1.0, (-0.9, -0.9)),
                   (Color3 1.0 1.0 0.0, (-0.9,  0.9))]
     renderPrimitive Polygon $
       forM_ points $ \(c, (x, y)) ->
         do color c
            vertex $ Vertex2 x y
     flush
