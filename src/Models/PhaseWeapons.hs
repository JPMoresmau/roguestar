module Models.PhaseWeapons
    (phase_pistol)
    where

import Quality
import Models.ConcordanceMaterials
import RSAGL.Vector
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.Affine
import RSAGL.CurveExtras
import RSAGL.Angle

phase_weapon_emitter :: Quality -> Modeling ()
phase_weapon_emitter _ = model $
    do sor $ linearInterpolation $
                points2d [(0.001,0),
                          (4,1),
                          (7,2),
                          (9,3),
                          (10,0),
                          (9,-3),
                          (7,-6),
                          (4,-9),
                          (2,-10),
                          (0.001,-10)]
       affine $ rotateX (fromDegrees 90)
       deform dfn
    where dfn_squish p@(Point3D _ _ z) | z > 0 = scale (Vector3D 1 1 0.1) p
          dfn_squish p = scale (Vector3D 1 1 0.25) p
          dfn_smoothe_dish p@(Point3D _ y _) | y > 0 = scale (Vector3D 1 (u/10) 1) p
              where u = distanceBetween origin_point_3d $ scale (Vector3D 1 0 1) p
          dfn_smoothe_dish p = p
          dfn = dfn_smoothe_dish . dfn_squish
                                 
phase_weapon_grip :: Quality -> Modeling ()
phase_weapon_grip _ = model $
    do sor $ linearInterpolation $
                points2d [(3,0),
                          (2,-4),
                          (1,-7),
                          (1,-10),
                          (0.001,-10)]
       deform dfn
   where dfn_flat_back p@(Point3D _ _ z) | z < 0 = scale (Vector3D (1/r) 1 (1/r)) p 
             where r = distanceBetween origin_point_3d $ scale (Vector3D 1 0 1) p
         dfn_flat_back p = p
         dfn_gripped_front p@(Point3D _ y z) | z > 0 = scale (Vector3D u 1 u) p 
             where u = 1 + (y * sin (pi*y) / 40)
         dfn_gripped_front p = p
         dfn_flat_sides (Point3D x y z) = Point3D ((signum x *) $ sqrt (abs x/3)) y z
         dfn_slope_backwards pt@(Point3D _ y _) = translate (Vector3D 0 0 (-y/3)) pt
         dfn = dfn_slope_backwards . dfn_flat_back . dfn_flat_sides . dfn_gripped_front
                                
phase_pistol :: Quality -> Modeling ()
phase_pistol q = model $
    scale' 0.02 $
        do translate (Vector3D 0 5 7) $ phase_weapon_emitter q
           translate (Vector3D 0 5 0) $ phase_weapon_grip q
	   concordance_metal
