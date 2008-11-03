module Models.CyborgType4
    (cyborg_type_4_dome,
     cyborg_type_4_base,
     cyborg_type_4_hyperspace_disc,
     cyborg_type_4_hyperspace_rotor,
     cyborg_type_4_hyperspace_stabilizer)
   where

import Models.Materials
import RSAGL.Vector
import RSAGL.Model
import RSAGL.CurveExtras
import Quality
import RSAGL.Affine
import RSAGL.ModelingExtras
import RSAGL.Interpolation
import RSAGL.Angle
import RSAGL.CoordinateSystems

cyborg_type_4_dome :: Quality -> Modeling ()
cyborg_type_4_dome q =
    do hemisphere (Point3D 0 0 0) (Vector3D 0 1 0) 9
       closedDisc (Point3D 0 0 0) (Vector3D 0 (-1) 0) 9.01
       affine $ scale $ Vector3D 1 (8/9) 1
       material cyborg_metal
       qualityToFixed q

cyborg_type_4_base :: Quality -> Modeling ()
cyborg_type_4_base q =
    do model $ 
           do openDisc (Point3D 0 0 0) (Vector3D 0 1 0) 0 10.01
              openCone (Point3D 0 (-6) 0,10) (Point3D 0 0 0,10)
              reverseOrientation $ openCone (Point3D 0 (-3) 0,9)  (Point3D 0 (-5) 0,9)
              reverseOrientation $ openCone (Point3D 0 (-5) 0,7)  (Point3D 0 (-6) 0,7)
              openDisc (Point3D 0 (-6) 0) (Vector3D 0 (-1) 0) 6.99 10.01
              qualityToFixed q
       sor $ linearInterpolation $ map ($ 0) $ reverse [
           Point3D 9 (-3),
	   Point3D 4 (-5),
	   Point3D 3 (-6),
	   Point3D 1 (-16),
	   Point3D 0 (-26)]
       material cyborg_metal

cyborg_type_4_hyperspace_disc :: Quality -> Modeling ()
cyborg_type_4_hyperspace_disc q =
    do closedCone (Point3D 0 0 0,10) (Point3D 0 10 0,10)
       material cyborg_glow
       qualityToFixed q

cyborg_type_4_hyperspace_rotor :: Quality -> Modeling ()
cyborg_type_4_hyperspace_rotor _ =
    do box (Point3D 15 4 (-0.5)) (Point3D 20 (-6) 0.5)
       material cyborg_glow
       fixed (3,3)

cyborg_type_4_hyperspace_stabilizer :: Quality -> Modeling ()
cyborg_type_4_hyperspace_stabilizer q =
    do box (Point3D 6 (-9) (-2)) (Point3D 10 (-16) 2)
       affine $ transformAbout (Point3D 8 (-16) 0) $ rotate (Vector3D 0 1 0) (fromDegrees 45)
       deform $ \(Point3D x _ _) -> Affine $ transformAbout (Point3D 10 (-16) 0) $ scaleAlong (Vector3D 0 1 0) $ lerpBetweenClampedMutated (^2) (6,x,10) (3/7,1)
       disregardSurfaceNormals
       material cyborg_glow
       qualityToFixed q
