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
import RSAGL.Types

-- | Rays with endpoints and vectors.
--
-- Although a ray is isomorphic to a 'SurfaceVertex3D', it does not have the
-- same behavior.
--
data Ray3D = Ray3D { ray_endpoint :: Point3D,
                     ray_vector :: Vector3D }
    deriving (Read,Show)

instance AffineTransformable Ray3D where
    transform m (Ray3D p v) = Ray3D (transform m p) (transform m v)

-- | The parametric function of a ray.  The parameter is measured as a
-- proportion of the length of the vector.  @projectRay 0@ is the
-- endpoint of the ray.  @projectRay 1@ is the endpoint offset
-- by the ray's vector.
projectRay :: RSdouble -> Ray3D -> Point3D
projectRay t (Ray3D (Point3D x y z) (Vector3D u v w)) = Point3D (x+u*t) (y+v*t) (z+w*t)

-- | The inverse operation to 'projectRay'.  This could also be
-- understood as the height of the point above the plane defined
-- by the ray.
distanceAlong :: Ray3D -> Point3D -> RSdouble
distanceAlong (Ray3D p v) p' = dotProduct (vectorToFrom p' p) v / vectorLengthSquared v

-- | The angle between vector of the ray and the vector from the
-- endpoint of the ray to the specified point.
angleFrom :: Ray3D -> Point3D -> Angle
angleFrom (Ray3D p v) p' = angleBetween (vectorToFrom p' p) v

-- | A ray normalize to a length of 1.
normalizeRay :: Ray3D -> Ray3D
normalizeRay (Ray3D p v) = Ray3D p $ vectorNormalize v

