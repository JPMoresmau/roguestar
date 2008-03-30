module Models.MachineParts
    (machine_arm_lower,
     machine_arm_upper)
    where
    
import Quality
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.CurveExtras
import RSAGL.Affine
import RSAGL.Vector
import RSAGL.Angle
import Models.AllianceMaterials

machine_arm_lower :: Quality -> Modeling ()
machine_arm_lower _ = scale' (recip 4.0) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $
    do sor $ linearInterpolation $
        points2d [(0.001,4.5),
                  (0.25,4.5),
                  (0.25,3.5),
                  (0.35,3),
                  (0.35,0.5),
                  (0.001,0.3)]
       alliance_metal

machine_arm_upper :: Quality -> Modeling ()
machine_arm_upper _ = scale' (recip 4.0) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $
    do sor $ linearInterpolation $
        points2d [(0.001,4.5),
                  (0.5,4.5),
                  (0.75,3.5),
                  (0.5,3),
                  (0.5,1),
                  (0.001,-0.5)]
       alliance_metal
