module Models.Stargate
    (portal)
    where

import RSAGL.Math
import RSAGL.Modeling
import Models.Materials
import Quality

portal :: Quality -> Modeling ()
portal q =
    do model $
           do model $
                  do box (Point3D (-0.6) 0 (-0.05)) (Point3D (-0.5) 1.618 0.05)
                     box (Point3D 0.6 0 (-0.05)) (Point3D 0.5 1.618 0.05)
                     box (Point3D (-1.1) 1.568 (-0.05))
                         (Point3D (1.1) 1.668 0.05)
                     minorFixedQuality q
              let cone =
                      do openCone (Point3D 0 (1.618/6) 0,0.4)
                                  (Point3D 0 (1.618/3) 0,0)
                         openCone (Point3D 0 (1.618/6) 0,0.4) (Point3D 0 0 0,0)
                  cone_stack = scale (Vector3D 1 (7/8) 1) $
                      do cone
                         translate (Vector3D 0 (1.618/3) 0) cone
                         translate (Vector3D 0 (2*1.618/3) 0) cone
                         translate (Vector3D 0 1.618 0) cone
              translate (Vector3D 1.1 0 0) cone_stack
              translate (Vector3D (-1.1) 0 0) cone_stack
              material treaty_metal
       model $
           do quadralateral (Point3D (-0.55) 0 0)
                            (Point3D (-0.55) 1.618 0)
                            (Point3D 0.55 1.618 0)
                            (Point3D 0.55 0 0)
              twoSided True
              material treaty_energy_field

