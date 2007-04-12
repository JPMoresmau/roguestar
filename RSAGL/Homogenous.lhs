\section{Representing objects in homogenous coordinates: RSAGL.Homogenous}

Entities such as points and vectors that can be represented as matrices.

toHomogenous always results in a column matrix, while fromHomogenous always expects a row matrix.
This means that (fromHomogenous . toHomogenous) is not an identity function.
Instead, (fromHomogenous . matrixTranspose . toHomogenous) is an identity function.

\begin{code}
module RSAGL.Homogenous
    (Homogenous(..))
    where

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

genericFromHomogenous :: Matrix Double -> (Double,Double,Double)
genericFromHomogenous m = let x = (row_major m) !! 0 !! 0
			      y = (row_major m) !! 1 !! 0
			      z = (row_major m) !! 2 !! 0
			      in (x,y,z)
\end{code}