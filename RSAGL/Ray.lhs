\section{Ray.hs}

\begin{code}
module RSAGL.Ray
    (Ray3D(..),
     projectRay,
     distanceAlong,
     angleFrom,
     normalizeRay) 
    where

import RSAGL.Vector
import RSAGL.Angle
import RSAGL.Affine
\end{code}

\subsection{Rays in 3-space}

A \texttt{Ray3D} has a endpoint and direction.

\begin{code}
data Ray3D = Ray3D Point3D Vector3D
    deriving (Read,Show)

instance AffineTransformable Ray3D where
    transform m (Ray3D p v) = Ray3D (transform m p) (transform m v)

projectRay :: Double -> Ray3D -> Point3D
projectRay t (Ray3D p v) = translate (vectorScale t v) p

distanceAlong :: Ray3D -> Point3D -> Double
distanceAlong (Ray3D p v) p' = dotProduct (vectorToFrom p' p) v / vectorLengthSquared v

angleFrom :: Ray3D -> Point3D -> Angle
angleFrom (Ray3D p v) p' = angleBetween (vectorToFrom p' p) v

normalizeRay :: Ray3D -> Ray3D
normalizeRay (Ray3D p v) = Ray3D p $ vectorNormalize v
\end{code}

