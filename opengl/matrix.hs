module Matrix (
  Matrix,
  identity,
  multiply,
  translate,
  rotate,
  scale,
  ortho,
  frustum,
  perspective,
  withMatrix,
  showMatrix
) where

import Control.Monad
import Data.Array.Storable
import Data.List
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL hiding (
  Matrix, frustum, newMatrix, ortho, perspective, rotate, scale, translate, withMatrix)

newtype Matrix e = Matrix (StorableArray Int e)

identity :: (Storable e, Num e) => IO (Matrix e)
identity = newMatrix [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]

multiply :: (Storable e, Num e) => Matrix e -> Matrix e -> IO ()
multiply (Matrix t) (Matrix m) =
  forM_ [[0, 4, 8, 12], [1, 5, 9, 13], [2, 6, 10, 14], [3, 7, 11, 15]] $ \v ->
    do [t0, t1, t2, t3] <- mapM (readArray t) v
       forM_ (zip v [0, 4, 8, 12]) $ \(n, i) ->
         do [m0, m1, m2, m3] <- mapM (readArray m) [i .. i + 3]
            writeArray t n $ t0*m0 + t1*m1 + t2*m2 + t3*m3

translate :: (Storable e, Num e) => Matrix e -> e -> e -> e -> IO ()
translate m x y z =
  do t <- newMatrix [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, x, y, z, 1]
     multiply m t

rotate :: (Storable e, Floating e) => Matrix e -> e -> e -> e -> e -> IO ()
rotate m angle x y z =
  do let mag = sqrt $ x*x + y*y + z*z
         nx = x/mag
         ny = y/mag
         nz = z/mag
         s = sin $ angle*pi/180
         c = cos $ angle*pi/180
     t <- newMatrix [x*x*(1 - c) + c,
                     y*x*(1 - c) + z*s,
                     x*z*(1 - c) - y*s,
                     0,
                     x*y*(1 - c) - z*s,
                     y*y*(1 - c) + c,
                     y*z*(1 - c) + x*s,
                     0,
                     x*z*(1 - c) + y*s,
                     y*z*(1 - c) - x*s,
                     z*z*(1 - c) + c,
                     0,
                     0,
                     0,
                     0,
                     1]
     multiply m t

scale :: (Storable e, Num e) => Matrix e -> e -> e -> e -> IO ()
scale m x y z =
  do t <- newMatrix [x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1]
     multiply m t

ortho :: (Storable e, Floating e) => Matrix e -> e -> e -> e -> e -> e -> e -> IO ()
ortho m left right bottom top near far =
  do t <- newMatrix [2/(right - left), 
                     0, 
                     0, 
                     0,
                     0, 
                     2/(top - bottom),
                     0, 
                     0,
                     0, 
                     0, 
                     -2/(far - near), 
                     0,
                     -((right + left)/(right - left)), 
                     -((top + bottom)/(top - bottom)), 
                     -((far + near)/(far - near)), 
                     1]
     multiply m t

frustum :: (Storable e, Floating e) => Matrix e -> e -> e -> e -> e -> e -> e -> IO ()
frustum m left right bottom top near far =
  do t <- newMatrix [2*near/(right - left),
                     0,
                     0,
                     0,
                     0,
                     2*near/(top - bottom),
                     0,
                     0,
                     (right + left)/(right - left),
                     (top + bottom)/(top - bottom),
                     -(far + near)/(far - near),
                     -1,
                     0,
                     0,
                     -2*far*near/(far - near),
                     0]
     multiply m t

perspective :: (Storable e, Floating e) => Matrix e -> e -> e -> e -> e -> IO ()
perspective m fovy aspect near far =
  do t <- newMatrix [(1/tan(fovy*pi/180/2))/aspect,
                     0,
                     0,
                     0,
                     0,
                     1/tan(fovy*pi/180/2),
                     0,
                     0,
                     0,
                     0,
                     (far + near)/(near - far),
                     -1,
                     0,
                     0,
                     (2*far*near)/(near - far),
                     0]
     multiply m t

newMatrix :: Storable e => [e] -> IO (Matrix e)
newMatrix = liftM Matrix . newListArray (0, 15)

withMatrix :: Storable e => Matrix e -> (Ptr e -> IO a) -> IO a
withMatrix (Matrix m) = withStorableArray m

showMatrix :: (Storable e, Show e) => Matrix e -> IO String
showMatrix (Matrix m) =
  do elems <- getElems m
     return $ intercalate "," $ map show elems
