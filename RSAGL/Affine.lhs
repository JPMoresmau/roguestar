\section{Transforming geometric objects: RSAGL.Affine}

AffineTransformable objects are entities that are subject to affine transformations using matrix multiplication.

Defaults are provided for all methods of AffineTransformable except transform.

The IO monad itself is AffineTransformable.  This is done by wrapping the IO action in an OpenGL transformation.

\begin{code}
module RSAGL.Affine
    (AffineTransformable(..),
     rotateX,
     rotateY,
     rotateZ,
     scale')
    where

import Graphics.Rendering.OpenGL.GL as GL
import RSAGL.Vector
import RSAGL.Matrix
import RSAGL.Angle
import RSAGL.Homogenous

class AffineTransformable a where
    transform :: RSAGL.Matrix.Matrix Double -> a -> a
    scale :: Vector3D -> a -> a
    scale vector = transform $ scaleMatrix vector
    translate :: Vector3D -> a -> a
    translate vector = transform $ translationMatrix vector
    rotate :: Vector3D -> Angle -> a -> a
    rotate vector angle = transform $ rotationMatrix vector angle

instance AffineTransformable a => AffineTransformable [a] where
    transform m = map (transform m)

instance (AffineTransformable a,AffineTransformable b) => AffineTransformable (a,b) where
    transform m (a,b) = (transform m a,transform m b)

instance (Fractional a,Real a,MatrixType a) => AffineTransformable (RSAGL.Matrix.Matrix a) where
    transform mat = coerceMatrix realToFrac . matrixMultiply mat . coerceMatrix realToFrac

instance AffineTransformable Vector3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1+x2) (y1+y2) (z1+z2)

instance AffineTransformable Point3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1+x2) (y1+y2) (z1+z2)

instance AffineTransformable Point2D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 _) (Point2D x2 y2) = Point2D (x1*x2) (y1*y2)
    translate (Vector3D x1 y1 _) (Point2D x2 y2) = Point2D (x1+x2) (y1+y2)

instance AffineTransformable (IO a) where
    transform mat iofn = preservingMatrix $ do mat' <- newMatrix RowMajor $ concat $ rowMajorForm mat
                                               multMatrix (mat' :: GLmatrix Double)
                                               iofn
    translate (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.translate $ Vector3 x y z
           iofn
    scale (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.scale x y z
           iofn
    rotate (Vector3D x y z) angle iofn = preservingMatrix $ 
        do GL.rotate (toDegrees angle) (Vector3 x y z)
           iofn
\end{code}

\subsection{Common transformations}

\begin{code}
rotateX :: (AffineTransformable a) => Angle -> a -> a
rotateX = RSAGL.Affine.rotate (Vector3D 1 0 0)

rotateY :: (AffineTransformable a) => Angle -> a -> a
rotateY = RSAGL.Affine.rotate (Vector3D 0 1 0)

rotateZ :: (AffineTransformable a) => Angle -> a -> a
rotateZ = RSAGL.Affine.rotate (Vector3D 0 0 1)

scale' :: (AffineTransformable a) => Double -> a -> a
scale' x = RSAGL.Affine.scale (Vector3D x x x)
\end{code}