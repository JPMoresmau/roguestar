module Models.MachineParts
    (machine_arm_lower,
     machine_arm_upper,
     thin_limb)
    where
    
import Quality
import RSAGL.Model
import RSAGL.CurveExtras
import RSAGL.Affine
import RSAGL.Vector
import RSAGL.Angle
import Models.Materials

machine_arm_lower :: Quality -> Modeling ()
machine_arm_lower _ = scale' (1/4) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $ reverse $ 
        points2d [(0.0,4.5),
                  (0.25,4.5),
                  (0.25,3.5),
                  (0.35,3),
                  (0.35,0.5),
                  (0.0,0.3)]
       alliance_metal

machine_arm_upper :: Quality -> Modeling ()
machine_arm_upper _ = scale' (1/4) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $ model $
    do sor $ linearInterpolation $ reverse $
        points2d [(0.0,4.5),
                  (0.5,4.5),
                  (0.75,3.5),
                  (0.5,3),
                  (0.5,1),
                  (0.0,-0.5)]
       alliance_metal

thin_limb :: Quality -> Modeling ()
thin_limb _ =
    do openCone (Point3D 0 0 0,0.05) (Point3D 0 0 1,0.05)
       sphere (Point3D 0 0 0) 0.05
       sphere (Point3D 0 0 1) 0.05
       concordance_metal
