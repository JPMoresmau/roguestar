\section{Ray.hs}

\begin{code}
module RSAGL.Math.Ray
    (Ray3D(..),
     projectRay,
     distanceAlong,
     angleFrom,
     normalizeRay) 
    where

import RSAGL.Math.Vector
import RSAGL.Math.Angle
import RSAGL.Math.Affine
\end{code}

\subsection{Rays in 3-space}

A \texttt{Ray3D} has a endpoint and direction.

\begin{code}
data Ray3D = Ray3D { ray_endpoint :: Point3D,
                     ray_vector :: Vector3D }
    deriving (Read,Show)

instance AffineTransformable Ray3D where
    transform m (Ray3D p v) = Ray3D (transform m p) (transform m v)

projectRay :: Double -> Ray3D -> Point3D
projectRay t (Ray3D (Point3D x y z) (Vector3D u v w)) = Point3D (x+u*t) (y+v*t) (z+w*t)

distanceAlong :: Ray3D -> Point3D -> Double
distanceAlong (Ray3D p v) p' = dotProduct (vectorToFrom p' p) v / vectorLengthSquared v

angleFrom :: Ray3D -> Point3D -> Angle
angleFrom (Ray3D p v) p' = angleBetween (vectorToFrom p' p) v

normalizeRay :: Ray3D -> Ray3D
normalizeRay (Ray3D p v) = Ray3D p $ vectorNormalize v
\end{code}

