module Models.MachineParts
    (machine_shoulder,
     machine_arm)
    where
    
import Quality
import Model
import Math3D
    
machine_arm :: Float -> Texture -> Quality -> Model
machine_arm s t q = scale' (s/4.0) $
                         rotate (Vector3D 1 0 0) (pi/2) $
                         qualitySor q t $
                         points2d [(0,4.5),
                                   (0.25,4.5),
                                   (0.35,3.5),
                                   (0.25,3),
                                   (0.25,1),
                                   (0,-0.5)]

machine_shoulder :: Float -> Texture -> Quality -> Model
machine_shoulder s t q = scale' (s/4.0) $
                         rotate (Vector3D 1 0 0) (pi/2) $
                         qualitySor q t $
                         points2d [(0,4.5),
                                   (0.5,4.5),
                                   (0.75,3.5),
                                   (0.5,3),
                                   (0.5,1),
                                   (0,-0.5)]