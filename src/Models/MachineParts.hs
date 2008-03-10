module Models.MachineParts
    (machine_shoulder,
     machine_arm)
    where
    
import Quality
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.CurveExtras
import RSAGL.Affine
import RSAGL.Vector
import RSAGL.Angle

machine_arm :: Double -> Quality -> Modeling ()
machine_arm s _ = scale' (s/4.0) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $
    sor $ linearInterpolation $
        points2d [(0,4.5),
                  (0.25,4.5),
                  (0.35,3.5),
                  (0.25,3),
                  (0.25,1),
                  (0,-0.5)]

machine_shoulder :: Double -> Quality -> Modeling ()
machine_shoulder s _ = scale' (s/4.0) $ rotate (Vector3D 1 0 0) (fromDegrees 90) $
    sor $ linearInterpolation $
        points2d [(0,4.5),
                  (0.5,4.5),
                  (0.75,3.5),
                  (0.5,3),
                  (0.5,1),
                  (0,-0.5)]
