module Models.PhaseWeapons
    (phase_pistol,
     phaser,
     phase_rifle)
    where

import Quality
import Models.Materials
import RSAGL.Math
import RSAGL.Modeling
import RSAGL.Math.CurveExtras
import Control.Monad

-- | A phase weapon emitter.  Includes a specification of a phase guide length.
phaseWeaponEmitter :: Integer -> Quality -> Modeling ()
phaseWeaponEmitter guide_length _ = model $
    do sor $ linearInterpolation $
                points2d $ reverse 
		         [(0,0),
                          (1,0),
                          (4,1),
                          (7,2),
                          (9,3),
                          (10,0),
                          (9,-3),
                          (7,-6),
                          (4,-9),
                          (2,-10),
                          (0,-10)]
       let guide_length_y = fromInteger guide_length * 3 + 6
       when (guide_length > 0) $ openCone (Point3D 0 0 8,1) (Point3D 0 guide_length_y 8,0)
       _ <- flip mapM [2..guide_length] $ \i ->
           do let y = fromInteger i * 3
              translate (Vector3D 0 y 0) $ torus 8 (lerpBetween (0,y,guide_length_y) (1,0))
       deform dfn_squish
       affine $ rotateX (fromDegrees 90)
    where dfn_squish p@(Point3D _ _ z) | z > 0 = scale (Vector3D 1 1 0.1) p  -- bottom of emitter is flattened
          dfn_squish p = scale (Vector3D 1 1 0.25) p                         -- top of emitter is more round
                                 
phase_weapon_grip :: Quality -> Modeling ()
phase_weapon_grip _ = model $
    do sor $ linearInterpolation $
                points2d $ reverse
		         [(3,0),
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

phaseWeapon :: Integer -> Quality -> Modeling ()
phaseWeapon guide_length q = model $
    scale' (1/100) $
        do translate (Vector3D 0 5 7) $ phaseWeaponEmitter guide_length q
           translate (Vector3D 0 5 0) $ phase_weapon_grip q
	   concordance_metal

phase_pistol :: Quality -> Modeling ()
phase_pistol = phaseWeapon 0

phaser :: Quality -> Modeling ()
phaser = phaseWeapon 1

phase_rifle :: Quality -> Modeling ()
phase_rifle = phaseWeapon 2
