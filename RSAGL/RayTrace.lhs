\section{Ray Tracing Support}

This section implements functions that could form the basis of a ray tracer.

\begin{code}
module RSAGL.RayTrace
    (Ray3D,
     ray3d,
     Geometry(..),
     Plane,
     plane,
     plane3,
     shadowDeform)
    where

import RSAGL.Affine
import RSAGL.Vector
import Data.Ord
import Data.List
import Data.Maybe
\end{code}

\subsection{Rays in 3-space}

A \texttt{Ray3D} has a endpoint and direction.

\begin{code}
data Ray3D = Ray3D Point3D Vector3D
    deriving (Read,Show)

projectRay :: Double -> Ray3D -> Point3D
projectRay t (Ray3D p v) = translate (vectorScale t v) p

ray3d :: Point3D -> Vector3D -> Ray3D
ray3d p v = Ray3D p $ vectorNormalize v
\end{code}

\subsection{Geometry}

\texttt{Geometry} supports testing ray-object intersections.  \texttt{traceRay} takes an incomming ray of unit length and the \texttt{Geometry} and yields both a \texttt{SurfaceVertex3D} for the point of intersection and the distance between the point of origin and the point of intersection.

\begin{code}
class Geometry g where
    testRay :: Ray3D -> g -> Maybe (Double,SurfaceVertex3D)

instance (Geometry g) => Geometry [g] where
    testRay ray gs = case hits of
                           [] -> Nothing
                           _  -> Just $ minimumBy (comparing fst) hits
        where hits = mapMaybe (testRay ray) gs
\end{code}

\subsection{Planes}

\begin{code}
data Plane = Plane Point3D Vector3D

instance Geometry Plane where
    testRay (ray@(Ray3D r r')) (Plane p n) = case t of
            _ | t > 0.0 -> Just (t,SurfaceVertex3D (projectRay t ray) n)
            _           -> Nothing
        where k = dotProduct n $ vectorToFrom p r
              a = dotProduct n r'
              t = k/a

plane :: Point3D -> Vector3D -> Plane
plane p v = Plane p $ vectorNormalize v

plane3 :: Point3D -> Point3D -> Point3D -> Plane
plane3 p1 p2 p3 = plane p1 $ newell [p1,p2,p3]
\end{code}

\subsection{Algorithms}

\texttt{shadowDeform} constructs a deformation function using a geometry.  An existing surface is mapped to the surface of the geometry by casting the surface along 
parallel rays, as happens when a shadow is cast by rays of sunlight.

\begin{code}
shadowDeform :: (Geometry g) => Vector3D -> g -> SurfaceVertex3D -> SurfaceVertex3D
shadowDeform sunlight g (sv3d) = maybe sv3d snd $ testRay r g
    where r = ray3d (sv3d_position sv3d) sunlight
\end{code}