-- | Affine Transformations of Arbitrary Geometric Objects
module RSAGL.Math.Affine
    (AffineTransformable(..),
     scale',
     inverseTransform,
     withTransformation,
     transformAbout,
     translateToFrom,
     rotateToFrom,
     scaleAlong)
    where

import Graphics.Rendering.OpenGL.GL as GL hiding (R)
import RSAGL.Math.Vector
import RSAGL.Math.Matrix
import RSAGL.Math.Angle
import Data.Maybe
import Foreign.C.Types

-- | 'AffineTransformable' objects are subject to affine transformations using matrix multiplication.
class AffineTransformable a where
    -- | Apply an affine transformation, defined by a 4x4 matrix.  (This is the only required method.)
    transform :: RSAGL.Math.Matrix.Matrix -> a -> a
    -- | Scale an entity along the @x@ @y@ and @z@ axes.  For example, @scale (Vector3D 2 3 4)@ will make an object twice as wide, three times as tall,
    -- and four times as deep.  It may be helpful to think of the vector as a control point on the vertex of a unit cube.
    scale :: Vector3D -> a -> a
    scale vector = transform $ scaleMatrix vector
    -- | Translate an entity along the specified vector.
    translate :: Vector3D -> a -> a
    translate vector = transform $ translationMatrix vector
    -- | Rotate an entity about the origin, using the specified vector as the axis of rotation.
    -- See also 'transformAbout' to rotate around an arbitrary point.
    rotate :: Vector3D -> Angle -> a -> a
    rotate vector angle = transform $ rotationMatrix vector angle
    -- | Specific rotation around the x-axis.
    rotateX :: Angle -> a -> a
    rotateX = RSAGL.Math.Affine.rotate (Vector3D 1 0 0)
    -- | Specific rotation around the y-axis.
    rotateY :: Angle -> a -> a
    rotateY = RSAGL.Math.Affine.rotate (Vector3D 0 1 0)
    -- | Specific rotation around the z-axis.
    rotateZ :: Angle -> a -> a
    rotateZ = RSAGL.Math.Affine.rotate (Vector3D 0 0 1)

-- | Apply the inverse of an affine transformation, defined by a 4x4 matrix.
{-# INLINE inverseTransform #-}
inverseTransform :: (AffineTransformable a) => RSAGL.Math.Matrix.Matrix -> a -> a
inverseTransform m = transform (matrixInverse m)

-- | Specific scale preserving proportions.
{-# INLINE scale' #-}
scale' :: (AffineTransformable a) => Double -> a -> a
scale' x = RSAGL.Math.Affine.scale (Vector3D x x x)

-- | Apply a function under an affine transformation.  @withTransformation m id@ is an identity if @m@ is invertable.
{-# INLINE withTransformation #-}
withTransformation :: (AffineTransformable a) => RSAGL.Math.Matrix.Matrix -> (a -> a) -> a -> a
withTransformation m f = inverseTransform m . f . transform m

-- | Apply a function treating a particular point as the origin.  For example, combining 'transformAbout' with 'RSAGL.Math.Affine.rotate'
-- performs a rotation about an arbitrary point rather than the origin.
{-# INLINE transformAbout #-}
transformAbout :: (AffineTransformable a) => Point3D -> (a -> a) -> a -> a
transformAbout center f = withTransformation (translateToFrom origin_point_3d center identity_matrix) f

-- | Specific translation along the vector between two points.
-- This ordinary use is to set the second point as the center of a model (typically origin_point_3d)
-- and the first point as the desired position of the model.
{-# INLINE translateToFrom #-}
translateToFrom :: (AffineTransformable a) => Point3D -> Point3D -> a -> a
translateToFrom a b = RSAGL.Math.Affine.translate (vectorToFrom a b)

-- | Specific rotation along the shortest path that brings the second vector in line with the first.
{-# INLINE rotateToFrom #-}
rotateToFrom :: (AffineTransformable a) => Vector3D -> Vector3D -> a -> a
rotateToFrom u v = RSAGL.Math.Affine.rotate c a
    where c = vectorNormalize $ vectorScale (-1) $ fromMaybe (fst $ orthos u) $ aNonZeroVector $ crossProduct u v
          a = angleBetween u v

-- | Specific scale along an arbitary axis.
{-# INLINE scaleAlong #-}
scaleAlong :: (AffineTransformable a) => Vector3D -> Double -> a -> a
scaleAlong v u = withTransformation (rotateToFrom (Vector3D 0 1 0) v identity_matrix) (RSAGL.Math.Affine.scale (Vector3D 1 u 1))

instance AffineTransformable a => AffineTransformable (Maybe a) where
    transform m = fmap (transform m)

instance AffineTransformable a => AffineTransformable [a] where
    transform m = map (transform m)

instance (AffineTransformable a,AffineTransformable b) => AffineTransformable (a,b) where
    transform m (a,b) = (transform m a,transform m b)

instance (AffineTransformable a,AffineTransformable b,AffineTransformable c) => AffineTransformable (a,b,c) where
    transform m (a,b,c) = (transform m a,transform m b,transform m c)

instance AffineTransformable RSAGL.Math.Matrix.Matrix where
    transform mat = matrixMultiply mat

instance AffineTransformable Vector3D where
    transform m (Vector3D x y z) = transformHomogenous x y z 0 Vector3D m
    scale (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1*x2) (y1*y2) (z1*z2)
    translate _ = id
    rotateX a (Vector3D x y z) = Vector3D x (c*y-s*z) (c*z+s*y)
        where s = sine a
              c = cosine a
    rotateY a (Vector3D x y z) = Vector3D (c*x+s*z) y (c*z-s*x)
        where s = sine a
              c = cosine a
    rotateZ a (Vector3D x y z) = Vector3D (c*x-s*y) (c*y+s*x) z
        where s = sine a
              c = cosine a

instance AffineTransformable Point3D where
    transform m (Point3D x y z) = transformHomogenous x y z 1 Point3D m
    scale (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1*x2) (y1*y2) (z1*z2)
    translate (Vector3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (x1+x2) (y1+y2) (z1+z2)
    rotateX a (Point3D x y z) = Point3D x (c*y-s*z) (c*z+s*y)
        where s = sine a
              c = cosine a
    rotateY a (Point3D x y z) = Point3D (c*x+s*z) y (c*z-s*x)
        where s = sine a
              c = cosine a
    rotateZ a (Point3D x y z) = Point3D (c*x-s*y) (c*y+s*x) z
        where s = sine a
              c = cosine a

instance AffineTransformable SurfaceVertex3D where
    transform m (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Math.Affine.transform m p) (RSAGL.Math.Affine.transform (matrixTranspose $ matrixInverse m) v)
    translate vector (SurfaceVertex3D p v) = SurfaceVertex3D (RSAGL.Math.Affine.translate vector p) v

-- | The IO monad itself is AffineTransformable.  This is done by wrapping the IO action in an OpenGL transformation.
instance AffineTransformable (IO a) where
    transform mat iofn = preservingMatrix $ do mat' <- newMatrix RowMajor $ map realToFrac $ concat $ rowMajorForm mat
                                               multMatrix (mat' :: GLmatrix Double)
                                               iofn
    translate (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.translate $ Vector3 x y z
           iofn
    scale (Vector3D x y z) iofn = preservingMatrix $ 
        do GL.scale x y z
           iofn
    rotate (Vector3D x y z) angle iofn = preservingMatrix $ 
        do GL.rotate (toDegrees_ angle) (Vector3 (realToFrac x) (realToFrac y) (realToFrac z))
           iofn
