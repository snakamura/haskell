import Control.Monad
import Data.IORef
import Foreign.Ptr
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLUT

import qualified Matrix as M
import Util

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode, DoubleBuffered]
     window <- createWindow "Window"
     clearColor $= Color4 1.0 1.0 1.0 0.0
     texture Texture2D $= Enabled
     program <- join $ liftM2 createProgram (loadShader "opengl20.vsh") 
                                            (loadShader "opengl20.fsh")
     texture <- createTexture "texture.rgb" 256 256
     counterLoc <- get $ uniformLocation program "u_counter"
     mvpLoc <- get $ uniformLocation program "u_mvp"
     textureLoc <- get $ uniformLocation program "u_texture"
     positionLoc <- get $ attribLocation program "a_position"
     let vertices :: [GLfloat]
         vertices = concat $ do let n = 32
                                x <- [0 .. n - 1]
                                y <- [0 .. n - 1]
                                let l = -1.0 + 2*x/n
                                    t = 1.0 - 2*y/n
                                    r = -1.0 + 2*(x + 1)/n
                                    b = 1.0 - 2*(y + 1)/n
                                return [l, t,
                                        l, b,
                                        r, t,
                                        r, t,
                                        l, b,
                                        r, b]
     buffer <- createArrayBuffer vertices
     sizeRef <- newIORef $ Size 0 0
     counterRef <- newIORef 0
     displayCallback $= display (fromIntegral (length vertices `div` 2)) program counterLoc mvpLoc textureLoc positionLoc buffer texture sizeRef counterRef
     reshapeCallback $= Just (reshape sizeRef)
     timer counterRef
     mainLoop

display count program counterLoc mvpLoc textureLoc positionLoc buffer texture sizeRef counterRef =
  do clear [ColorBuffer]
     currentProgram $= Just program
     counter <- readIORef counterRef
     bindBuffer ArrayBuffer $= Just buffer
     glUniform1f (extractUniformLoc counterLoc) counter
     modelViewMatrix <- M.identity
     M.rotate modelViewMatrix 20 0.0 1.0 0.0
     M.scale modelViewMatrix 1.0 1.0 0.05
     projectionMatrix <- M.identity
     Size w h <- readIORef sizeRef
     M.perspective projectionMatrix 70 (fromIntegral w/fromIntegral h) (1.0) 10.0
     M.translate projectionMatrix 0.0 0.0 (-2.0)
     mvpMatrix <- M.identity
     M.multiply mvpMatrix projectionMatrix
     M.multiply mvpMatrix modelViewMatrix
     M.withMatrix mvpMatrix $ \ptr ->
       glUniformMatrix4fv (extractUniformLoc mvpLoc) 1 0 ptr
     textureBinding Texture2D $= Just texture
     textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
     vertexAttribPointer positionLoc $= (ToFloat, VertexArrayDescriptor 2 Float 0 (intPtrToPtr 0))
     vertexAttribArray positionLoc $= Enabled
     drawArrays Triangles 0 count
     swapBuffers

reshape sizeRef size@(Size w h) =
  do viewport $= (Position 0 0, size)
     writeIORef sizeRef size

timer counterRef =
  do modifyIORef counterRef (+1)
     postRedisplay Nothing
     addTimerCallback 60 $ timer counterRef
