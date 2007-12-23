\section{Transforming geometric objects: RSAGL.Affine}

AffineTransformable objects are entities that are subject to affine transformations using matrix multiplication.

Defaults are provided for all methods of AffineTransformable except transform.

The IO monad itself is AffineTransformable.  This is done by wrapping the IO action in an OpenGL transformation.

\begin{code}
{-# OPTIONS_GHC -fglasgow-exts #-}

module RSAGL.Affine
    (AffineTransformable(..),
     WrappedAffine(..),wrapAffine,unwrapAffine,
     transformation,
     inverseTransformation)
    where

import Graphics.Rendering.OpenGL.GL as GL
import RSAGL.Vector
import RSAGL.Matrix
import RSAGL.Angle
import RSAGL.Homogenous

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

transformation :: (AffineTransformable b) => (forall a. (AffineTransformable a) => a -> a) -> b -> b
transformation f = transform (f $ identityMatrix 4)

inverseTransformation :: (AffineTransformable b) => (forall a. (AffineTransformable a) => a -> a) -> b -> b
inverseTransformation f = transform (matrixInverse $ f $ identityMatrix 4)

instance AffineTransformable a => AffineTransformable [a] where
    scale v = map (RSAGL.Affine.scale v)
    translate v = map (RSAGL.Affine.translate v)
    rotate angle vector = map (RSAGL.Affine.rotate angle vector)
    transform m = map (transform m)

instance (AffineTransformable a,AffineTransformable b) => AffineTransformable (a,b) where
    transform m (a,b) = (transform m a,transform m b)

instance AffineTransformable RSAGL.Matrix.Matrix where
    transform mat = matrixMultiply mat

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

instance AffineTransformable SurfaceVertex3D where
    transform m (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.transform m p) (RSAGL.Affine.transform m v)
    scale vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.scale vector p) (RSAGL.Affine.scale vector v)
    translate vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Affine.translate vector p) (RSAGL.Affine.translate vector v)

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

\texttt{WrappedAffine} stores up affine transformations that are commited only when the entity is unwrapped.  This is an optimization when the entity in question is expensive
to transform.

\begin{code}
data WrappedAffine a = WrappedAffine (RSAGL.Matrix.Matrix) a

wrapAffine :: a -> WrappedAffine a
wrapAffine = WrappedAffine (identityMatrix 4)

unwrapAffine :: (AffineTransformable a) => WrappedAffine a -> a
unwrapAffine (WrappedAffine m a) = transform m a

instance AffineTransformable (WrappedAffine a) where
    transform t (WrappedAffine m a) = WrappedAffine (transform t m) a
\end{code}