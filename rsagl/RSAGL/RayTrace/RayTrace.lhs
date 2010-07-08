\section{Ray Tracing Support}

This section implements functions that could form the basis of a ray tracer.

\begin{code}
module RSAGL.RayTrace.RayTrace
    (Geometry(..),
     Plane,
     plane,
     plane3,
     UnitSphere(..), Sphere,
     sphere,
     testRay1st,
     shadowDeform)
    where

import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Affine
import RSAGL.Scene.WrappedAffine
import RSAGL.Math.Vector
import RSAGL.Math.Ray
import Data.Ord
import Data.List
import Data.Maybe
import RSAGL.Types
\end{code}

\subsection{Geometry}

\texttt{Geometry} supports testing ray-object intersections.  \texttt{traceRay} takes an incomming ray of unit length and the \texttt{Geometry} and yields both a \texttt{SurfaceVertex3D} for the point of intersection and the distance between the point of origin and all points of intersection.  A negative distance is valid and optional if the point of intersection is behind the ray.

\begin{code}
class Geometry g where
    testRay :: Ray3D -> g -> [(RSdouble,SurfaceVertex3D)]

instance (Geometry g) => Geometry [g] where
    testRay ray gs = concatMap (testRay ray) gs
\end{code}

\subsection{Planes}

\begin{code}
data Plane = Plane Point3D Vector3D

instance Geometry Plane where
    testRay (ray@(Ray3D r r')) (Plane p n) = case t of
            _ | t > 0.0 -> [(t,SurfaceVertex3D (projectRay t ray) n)]
            _           -> []
        where k = dotProduct n $ vectorToFrom p r
              a = dotProduct n r'
              t = k/a

instance AffineTransformable Plane where
    transform m (Plane p v) = Plane (transform m p) (transform m v)

plane :: Point3D -> Vector3D -> Plane
plane p v = Plane p $ vectorNormalize v

plane3 :: Point3D -> Point3D -> Point3D -> Plane
plane3 p1 p2 p3 = plane p1 $ fromMaybe (error $ "plane3: " ++ show (p1,p2,p3) ++ " don't uniquely define a plane.") $ newell [p1,p2,p3]
\end{code}

\subsection{Spheres}

\begin{code}
data UnitSphere = UnitSphere
type Sphere = WrappedAffine UnitSphere

instance Geometry UnitSphere where
    testRay (ray@(Ray3D (Point3D kx ky kz) (Vector3D vx vy vz))) UnitSphere = 
        let a = vx^2 + vy^2 + vz^2
	    b = 2 * (vx*kx + vy*ky + vz*kz)
	    c = kx^2  + ky^2  + kz^2 - 1
	    p2s (Point3D x y z) = SurfaceVertex3D (Point3D x y z) (Vector3D x y z)
	    in case highSchoolAlgebra a b c of
		Just (Right (x,y)) -> [(x,p2s $ projectRay x ray),(y,p2s $ projectRay y ray)]
		_ -> []

highSchoolAlgebra :: RSdouble -> RSdouble -> RSdouble -> Maybe (Either RSdouble (RSdouble,RSdouble))
highSchoolAlgebra a b c =
    let d = b*b - 4*a*c
        sqrtd = sqrt d
        ta = 2*a
        in case () of
	    () | d == 0 -> Just $ Left $ negate b / ta
	    () | d > 0 -> Just $ Right ((negate b + sqrtd) / ta,(negate b - sqrtd) / ta)
	    () -> Nothing

sphere :: Point3D -> RSdouble -> Sphere
sphere p r = translateToFrom p origin_point_3d $ scale' r $ wrapAffine UnitSphere
\end{code}

\subsection{Instances}

\begin{code}
instance Geometry g => Geometry (WrappedAffine g) where
    testRay r (WrappedAffine m g) = map (\(_,sv3d) -> let SurfaceVertex3D p v = migrateToFrom m root_coordinate_system sv3d
            in (distanceAlong r p,SurfaceVertex3D p v)) $
        testRay (normalizeRay $ migrateToFrom root_coordinate_system m r) g
\end{code}

\subsection{Algorithms}

\texttt{testRay1st} is the special case of testRay that returns only the nearest point of intersection, if any.

\begin{code}
testRay1st :: (Geometry g) => Ray3D -> g -> Maybe (RSdouble,SurfaceVertex3D)
testRay1st r g = listToMaybe $ sortBy (comparing fst) $ filter ((>0) . fst) $ testRay r g
\end{code}

\texttt{shadowDeform} constructs a deformation function using a geometry.  An existing surface is mapped to the surface of the geometry by casting the surface along 
parallel rays, as happens when a shadow is cast by rays of sunlight.

\begin{code}
shadowDeform :: (Geometry g) => Vector3D -> g -> SurfaceVertex3D -> SurfaceVertex3D
shadowDeform sunlight g (sv3d) = maybe sv3d snd $ testRay1st r g
    where r = Ray3D (sv3d_position sv3d) sunlight
\end{code}
