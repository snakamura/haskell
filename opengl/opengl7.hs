import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     displayCallback $= display
     reshapeCallback $= Just reshape
     mainLoop

display =
  do clear [ColorBuffer]
     let points :: [(Color3 GLfloat, (GLfloat, GLfloat))]
         points = [(Color3 1.0 0.0 0.0, ( 0.9,  0.9)),
                   (Color3 0.0 1.0 0.0, ( 0.9, -0.9)), 
                   (Color3 0.0 0.0 1.0, (-0.9, -0.9)),
                   (Color3 1.0 1.0 0.0, (-0.9,  0.9))]
     preservingMatrix $
       do translate $ (Vector3 0.5 0.5 0 :: Vector3 GLfloat)
          rotate 30 $ (Vector3 0.0 0.0 1.0 :: Vector3 GLfloat)
          translate $ (Vector3 (-0.5) (-0.5) 0 :: Vector3 GLfloat)
          renderPrimitive Polygon $
            forM_ points $ \(c, (x, y)) ->
              do color c
                 vertex $ Vertex2 x y
     flush

reshape size@(Size w h) =
  do viewport $= (Position 0 0, size)
     loadIdentity
     ortho ((negate $ fromIntegral w)/200) 
           (fromIntegral w/200) 
           ((negate $ fromIntegral h)/200) 
           (fromIntegral h/200) 
           (-1) 
           1
