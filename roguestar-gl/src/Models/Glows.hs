module Models.Glows
    (ascendant_glow,
     dust_puff)
    where

import RSAGL.Modeling
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import RSAGL.Math
import Quality

ascendant_glow :: Quality -> Modeling ()
ascendant_glow _ = model $
    do closedDisc (Point3D 0 0 0) (Vector3D 0 1 0) 0.25
       material $ emissive $ scalarMultiply (1/3) <$> pattern (spherical (Point3D 0 0 0) 0.25 )
                      [(0.0,pure white),(0.25,pure light_blue),(1.0,pure blackbody)]
       affine $ translate (Vector3D 0 0.25 0)

dust_puff :: Quality -> Modeling ()
dust_puff _ = model $
    do closedDisc (Point3D 0 0 0) (Vector3D 0 1 0) 0.25
       material $
           do emissive $ scalarMultiply (1/5) <$> pattern (spherical (Point3D 0 0 0) 0.25 )
                   [(0.0,pure light_pink),(0.25,pure light_brown),(1.0,pure blackbody)]
              transparent $ pattern (spherical (Point3D 0 0 0) 0.25)
                   [(0.0,pure $ alpha 1.0 $ transformColor light_brown),
                    (1.0,pure $ alpha 0.0 $ transformColor light_brown)]
       affine $ translate (Vector3D 0 0.25 0)

