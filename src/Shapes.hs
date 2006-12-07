module Shapes
    (ring)
    where
    
import Math3D
    
-- |
-- Answers a ring of points on the XY plane 1 unit away from the origin.
--
ring :: Integer -> [Point3D]
ring n = map ((\x -> Point3D (cos x) (sin x) 0) . (2 * pi / fromInteger n *) . fromInteger) [0..(n-1)]