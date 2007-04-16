\section{Representing objects in homogenous coordinates: RSAGL.Homogenous}

Entities such as points and vectors that can be represented as matrices.  The Homogenous typeclass is
an easy way to implement affine transformations on these types.

toHomogenous always results in a column matrix, while fromHomogenous always expects a row matrix.
This means that (fromHomogenous . toHomogenous) is not an identity function.
Instead, (fromHomogenous . matrixTranspose . toHomogenous) is an identity function.

\begin{code}
module RSAGL.Homogenous
    (Homogenous(..),
     transformHomogenous)
    where

import RSAGL.Vector
import RSAGL.Matrix

class Homogenous a where
    toHomogenous :: a -> Matrix Double
    fromHomogenous :: Matrix Double -> a

instance Homogenous Vector3D where
    toHomogenous (Vector3D x y z) = matrix [[x],
                                            [y],
                                            [z],
                                            [0.0]]
    fromHomogenous m = vector3d $ genericFromHomogenous m 

instance Homogenous Point2D where
    toHomogenous (Point2D x y) = matrix [[x],
                                         [y],
                                         [0.0],
                                         [1.0]]
    fromHomogenous m = case (genericFromHomogenous m) of
						      (x,y,_) -> Point2D x y

instance Homogenous Point3D where
    toHomogenous (Point3D x y z) = matrix [[x],
                                           [y],
                                           [z],
                                           [1.0]]
    fromHomogenous m = point3d $ genericFromHomogenous m

genericFromHomogenous :: Matrix Double -> XYZ
genericFromHomogenous m = let x = (rowMajorForm m) !! 0 !! 0
			      y = (rowMajorForm m) !! 1 !! 0
			      z = (rowMajorForm m) !! 2 !! 0
			      in (x,y,z)

transformHomogenous :: (Homogenous a, Homogenous b) => Matrix Double -> a -> b
transformHomogenous transformation_matrix entity = 
    fromHomogenous $ matrixMultiply transformation_matrix $ toHomogenous entity
\end{code}