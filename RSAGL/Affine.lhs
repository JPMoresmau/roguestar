\section{Transforming geometric objects: RSAGL.Affine}

AffineTransformable objects are entities that are subject to affine transformations using matrix multiplication.
Transform is the only required function.  Scale, translate, and rotate may optionally be provided for efficiency.

\begin{code}
module RSAGL.Affine
    (AffineTransformable(..))
    where

import RSAGL.Vector

class AffineTransformable a where
    transform :: Matrix Double -> a -> a
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
                
instance (Fractional a,Real a,MatrixType a) => AffineTransformable (Matrix a) where
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
                
transformHomogenous :: (Homogenous a, Homogenous b) => Matrix Double -> a -> b
transformHomogenous transformation_matrix entity = 
    fromHomogenous $ matrixMultiply transformation_matrix $ toHomogenous entity

rotateX :: (AffineTransformable a) => Angle -> a -> a
rotateX = rotate (Vector3D 1 0 0)

rotateY :: (AffineTransformable a) => Angle -> a -> a
rotateY = rotate (Vector3D 0 1 0)

rotateZ :: (AffineTransformable a) => Angle -> a -> a
rotateZ = rotate (Vector3D 0 0 1)
                
scale' :: (AffineTransformable a) => Double -> a -> a
scale' x = scale (Vector3D x x x)