\section{Transforming geometric objects: RSAGL.Affine}

AffineTransformable objects are entities that are subject to affine transformations using matrix multiplication.

Defaults are provided for all methods of AffineTransformable except transform.

The IO monad itself is AffineTransformable.  This is done by wrapping the IO action in an OpenGL transformation.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Affine
    (AffineTransformable(..),
     transformAbout,
     translateToFrom,
     rotateToFrom)
    where

import Graphics.Rendering.OpenGL.GL as GL hiding (R)
import RSAGL.Vector
import RSAGL.Matrix
import RSAGL.Angle
import RSAGL.Homogenous
\end{code}

\begin{code}
class AffineTransformable a where
    transform :: RSAGL.Matrix.Matrix -> a -> a
    scale :: Vector3D -> a -> a
    scale vector = transform $ scaleMatrix vector
    translate :: Vector3D -> a -> a
    translate vector = transform $ translationMatrix vector
    rotate :: Vector3D -> Angle -> a -> a
    rotate vector angle = transform $ rotationMatrix vector angle
    rotateX :: Angle -> a -> a
    rotateX = RSAGL.Affine.rotate (Vector3D 1 0 0)
    rotateY :: Angle -> a -> a
    rotateY = RSAGL.Affine.rotate (Vector3D 0 1 0)
    rotateZ :: Angle -> a -> a
    rotateZ = RSAGL.Affine.rotate (Vector3D 0 0 1)
    scale' :: Double -> a -> a
    scale' x = RSAGL.Affine.scale (Vector3D x x x)
    inverseTransform :: RSAGL.Matrix.Matrix -> a -> a
    inverseTransform m = transform (matrixInverse m)
\end{code}

\texttt{transformAbout} performs an affine transformation treating a particular point as the origin.  For example,
combining \texttt{transformAbout} with \texttt{rotate} performs a rotation about an arbitrary point rather than the origin.

\begin{code}
transformAbout :: (AffineTransformable a) => Point3D -> (a -> a) -> a -> a
transformAbout center tform = 
    RSAGL.Affine.translate (vectorToFrom center origin_point_3d) .
    tform .
    RSAGL.Affine.translate (vectorToFrom origin_point_3d center)

translateToFrom :: (AffineTransformable a) => Point3D -> Point3D -> a -> a
translateToFrom a b = RSAGL.Affine.translate (vectorToFrom a b)

rotateToFrom :: (AffineTransformable a) => Vector3D -> Vector3D -> a -> a
rotateToFrom u v = RSAGL.Affine.rotate c a
    where c = vectorNormalize $ vectorScale (-1) $ crossProduct u v
          a = angleBetween u v

instance AffineTransformable a => AffineTransformable (Maybe a) where
    scale v = fmap (RSAGL.Affine.scale v)
    translate v = fmap (RSAGL.Affine.translate v)
    rotate angle vector = fmap (RSAGL.Affine.rotate angle vector)
    transform m = fmap (transform m)

instance AffineTransformable a => AffineTransformable [a] where
    scale v = map (RSAGL.Affine.scale v)
    translate v = map (RSAGL.Affine.translate v)
    rotate angle vector = map (RSAGL.Affine.rotate angle vector)
    transform m = map (transform m)

instance (AffineTransformable a,AffineTransformable b) => AffineTransformable (a,b) where
    transform m (a,b) = (transform m a,transform m b)

instance (AffineTransformable a,AffineTransformable b,AffineTransformable c) => AffineTransformable (a,b,c) where
    transform m (a,b,c) = (transform m a,transform m b,transform m c)

instance AffineTransformable RSAGL.Matrix.Matrix where
    transform mat = matrixMultiply mat

instance AffineTransformable Vector3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1*x2) (y1*y2) (z1*z2)
    translate _ = id

instance AffineTransformable Point3D where
    transform = transformHomogenous
    scale (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1+x2) (y1+y2) (z1+z2)

instance AffineTransformable SurfaceVertex3D where
    transform m (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.transform m p) (RSAGL.Affine.transform (matrixTranspose $ matrixInverse m) v)
    translate vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.translate vector p) v

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
        do GL.rotate (toDegrees_ angle) (Vector3 x y z)
           iofn
\end{code}
