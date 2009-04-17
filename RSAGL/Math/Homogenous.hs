module RSAGL.Math.Homogenous
    (Homogenous(..),
     transformHomogenous)
    where

import RSAGL.Math.Vector
import RSAGL.Math.Matrix

-- | Entities such as points and vectors that can be represented as matrices.  The 'Homogenous' typeclass is
-- an easy way to implement affine transformations on these types.
-- 'toHomogenous' always results in a column matrix, while 'fromHomogenous' always expects a row matrix.
-- This means that @(fromHomogenous . toHomogenous)@ is not an identity function.
-- Instead, @(fromHomogenous . matrixTranspose . toHomogenous)@ is an identity function.
class Homogenous a where
    toHomogenous :: a -> Matrix
    fromHomogenous :: Matrix -> a

instance Homogenous Vector3D where
    toHomogenous (Vector3D x y z) = columnMatrix4 x y z 0
    fromHomogenous = fromRowMatrix3 Vector3D

instance Homogenous Point3D where
    toHomogenous (Point3D x y z) = columnMatrix4 x y z 1
    fromHomogenous = fromRowMatrix3 Point3D

{-# INLINE transformHomogenous #-}
transformHomogenous :: (Homogenous a, Homogenous b) => Matrix -> a -> b
transformHomogenous transformation_matrix = 
    fromHomogenous . matrixMultiply transformation_matrix . toHomogenous
