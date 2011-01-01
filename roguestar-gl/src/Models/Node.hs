module Models.Node
    (monolith,
     planetary_anchor_core,
     planetary_anchor_flange)
    where

import RSAGL.Math
import RSAGL.Modeling
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import Quality

monolith :: Quality -> Modeling ()
monolith _ = model $
    do box (Point3D (-1/2) 0 (-1/8)) (Point3D (1/2) (9/4) (1/8))
       material $
           do pigment $ pure blackbody
              specular 100 $ pure white

planetary_anchor_core :: Quality -> Modeling ()
planetary_anchor_core _ = model $
    do sphere (Point3D 0 0 0) 0.05
       material $ emissive $ pure $ grayscale 0.75

planetary_anchor_flange :: Quality -> Modeling ()
planetary_anchor_flange _ = model $
    do openDisc (Point3D 0 0 0)
                (Vector3D 0 1 0)
                0.20
                0.21
       material $ emissive $ pure violet
       twoSided True

