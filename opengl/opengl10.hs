import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode, WithDepthBuffer]
     createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     depthFunc $= Just Less
     cullFace $= Just Back
     displayCallback $= display
     reshapeCallback $= Just reshape
     mainLoop

display =
  do clear [ColorBuffer, DepthBuffer]
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
         faces :: [((Int, Int, Int, Int), Color3 GLfloat)]
         faces = [((3, 2, 1, 0), Color3 1.0 0.0 0.0),
                  ((2, 6, 5, 1), Color3 0.0 1.0 0.0),
                  ((6, 7, 4, 5), Color3 0.0 0.0 1.0),
                  ((7, 3, 0, 4), Color3 1.0 1.0 0.0),
                  ((0, 1, 5, 4), Color3 1.0 0.0 1.0),
                  ((7, 6, 2, 3), Color3 0.0 1.0 1.0)]
--     rotate 30 (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
     preservingMatrix $
       do renderPrimitive Quads $
            forM_ faces $ \((p, q, r, s), c) ->
              do let v n = let (x, y, z) = points !! n
                           in Vertex3 x y z
                 color c
                 mapM_ (vertex . v) [p, q, r, s]
     flush

reshape size@(Size w h) =
  do viewport $= (Position 0 0, size)
     matrixMode $= Projection
     loadIdentity
     perspective 30.0 (fromIntegral w / fromIntegral h) 1.0 100.0
     lookAt (Vertex3 3.0 4.0 5.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
