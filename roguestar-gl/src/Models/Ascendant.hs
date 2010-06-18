module Models.Ascendant
    (ascendant_glow)
    where

import RSAGL.Modeling
import RSAGL.Math
import Quality

ascendant_glow :: Quality -> Modeling ()
ascendant_glow _ =
    do closedDisc (Point3D 0 0 0) (Vector3D 0 1 0) 0.25
       material $ emissive $ scaleRGB (1/5) <$> pattern (spherical (Point3D 0 0 0) 0.25 ) [(0.0,pure white),(0.25,pure light_blue),(1.0,pure blackbody)]
       affine $ translate (Vector3D 0 0.25 0)
