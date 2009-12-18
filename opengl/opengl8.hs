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
     color $ (Color3 0.0 0.0 0.0 :: Color3 GLfloat)
     let points :: [(GLfloat, GLfloat, GLfloat)]
         points = [(0.0, 0.0, 0.0),
                   (1.0, 0.0, 0.0),
                   (1.0, 1.0, 0.0),
                   (0.0, 1.0, 0.0),
                   (0.0, 0.0, 1.0),
                   (1.0, 0.0, 1.0),
                   (1.0, 1.0, 1.0),
                   (0.0, 1.0, 1.0)]
         edges :: [(Int, Int)]
         edges = [(0, 1),
                  (1, 2),
                  (2, 3),
                  (3, 0),
                  (4, 5),
                  (5, 6),
                  (6, 7),
                  (7, 4),
                  (0, 4),
                  (1, 5),
                  (2, 6),
                  (3, 7)]
     preservingMatrix $
       do rotate 30 (Vector3 1.0 1.0 1.0 :: Vector3 GLfloat)
          renderPrimitive Lines $
            forM_ edges $ \(s, e) ->
              do let (sx, sy, sz) = points !! s
                     (ex, ey, ez) = points !! e
                 vertex $ Vertex3 sx sy sz
                 vertex $ Vertex3 ex ey ez
     flush

reshape size@(Size w h) =
  do viewport $= (Position 0 0, size)
     matrixMode $= Projection
     loadIdentity
     ortho (-2.0) 2.0 (-2.0) 2.0 (-2.0) 2.0
