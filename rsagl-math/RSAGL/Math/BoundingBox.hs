module RSAGL.Math.BoundingBox
    (BoundingBox,
     Bound3D(..),
     boundingCenterRadius,
     minimalDistanceToBoundingBox)
   where

import RSAGL.Math.Vector
import RSAGL.Math.Interpolation
import RSAGL.Math.Affine
import RSAGL.Math.Types

-- | A simple bounding box.  Operations on bounding boxes
-- are designed to be fast, not accurate.  The only
-- guarantee on any bounding box operation is that
-- objects reported to be outside a bounding box,
-- are.
data BoundingBox = BoundingBox {
    bbox_bottom, bbox_top, bbox_left,
    bbox_right, bbox_far, bbox_near :: !RSdouble }
        deriving (Show)

-- | A convenience class for any finite geometry.
-- In particular, it's easy to concatenate the bounding
-- box of multiple geometries by placing them in a list
-- and taking the bounding box of the entire list.
class Bound3D a where
    boundingBox :: a -> BoundingBox

instance Bound3D Point3D where
    boundingBox (Point3D x y z) = BoundingBox {
                                      bbox_bottom = y,
                                      bbox_top = y,
                                      bbox_right = x,
                                      bbox_left = x,
                                      bbox_near = z,
                                      bbox_far = z }

instance Bound3D SurfaceVertex3D where
    boundingBox = boundingBox . sv3d_position

instance (Bound3D a) => Bound3D [a] where
    boundingBox [] = error "instance Bound3D [a], boundingBox: can't construct boundingBox []"
    boundingBox (x:xs) = foldr combineBoundingBoxes (boundingBox x) $ xs

instance Bound3D BoundingBox where
    boundingBox = id

instance AffineTransformable BoundingBox where
    transform m = boundingBox . transform m . boundingBoxToPointCloud

combineBoundingBoxes :: (Bound3D a,Bound3D b) => a -> b -> BoundingBox
combineBoundingBoxes x y =
    BoundingBox {
        bbox_left = min (bbox_left b1) (bbox_left b2),
        bbox_right = max (bbox_right b1) (bbox_right b2),
        bbox_bottom = min (bbox_bottom b1) (bbox_bottom b2),
        bbox_top = max (bbox_top b1) (bbox_top b2),
        bbox_near = min (bbox_near b1) (bbox_near b2),
        bbox_far = max (bbox_far b1) (bbox_far b2) }
    where b1 = boundingBox x
          b2 = boundingBox y

boundingBoxToPointCloud :: BoundingBox -> [Point3D]
boundingBoxToPointCloud bbox =
    [Point3D (bbox_left bbox)  (bbox_bottom bbox) (bbox_near bbox),
     Point3D (bbox_right bbox) (bbox_bottom bbox) (bbox_near bbox),
     Point3D (bbox_left bbox)  (bbox_top bbox)    (bbox_near bbox),
     Point3D (bbox_right bbox) (bbox_top bbox)    (bbox_near bbox),
     Point3D (bbox_left bbox)  (bbox_bottom bbox) (bbox_far bbox),
     Point3D (bbox_right bbox) (bbox_bottom bbox) (bbox_far bbox),
     Point3D (bbox_left bbox)  (bbox_top bbox)    (bbox_far bbox),
     Point3D (bbox_right bbox) (bbox_top bbox)    (bbox_far bbox)]

-- | View of a bounding box in the form of a bounding spehre.
boundingCenterRadius :: BoundingBox -> (Point3D,RSdouble)
boundingCenterRadius bbox = (lerp 0.5 (nlb,frt),distanceBetween nlb frt / 2)
    where nlb = Point3D (bbox_near bbox) (bbox_left bbox) (bbox_bottom bbox)
          frt = Point3D (bbox_far bbox) (bbox_right bbox) (bbox_top bbox)

-- | Estimates distance between a point and the outside surface of a bounding
-- box.  If the value is negative, then the point lies inside the bound
-- region.
minimalDistanceToBoundingBox :: Point3D -> BoundingBox -> RSdouble
minimalDistanceToBoundingBox p bbox = distanceBetween p c - r
    where (c,r) = boundingCenterRadius bbox

