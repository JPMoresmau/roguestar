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
    toHomogenous :: a -> Matrix
    fromHomogenous :: Matrix -> a

instance Homogenous Vector3D where
    toHomogenous (Vector3D x y z) = matrix [[x],
                                            [y],
                                            [z],
                                            [0.0]]
    fromHomogenous m = Vector3D (matrixAt m (0,0))
                                (matrixAt m (1,0))
				(matrixAt m (2,0))

instance Homogenous Point3D where
    toHomogenous (Point3D x y z) = matrix [[x],
                                           [y],
                                           [z],
                                           [1.0]]
    fromHomogenous m = Point3D (matrixAt m (0,0))
                               (matrixAt m (1,0))
			       (matrixAt m (2,0))

{-# INLINE transformHomogenous #-}
transformHomogenous :: (Homogenous a, Homogenous b) => Matrix -> a -> b
transformHomogenous transformation_matrix = 
    fromHomogenous . matrixMultiply transformation_matrix . toHomogenous
\end{code}
