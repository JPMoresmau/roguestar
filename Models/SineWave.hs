module Models.SineWave
    (sine_wave)
    where

import RSAGL.Model
import RSAGL.Angle
import RSAGL.Vector
import RSAGL.Affine

sine_wave :: Model
sine_wave =
    deformedSor
            (\x -> translate (Vector3D 0 (sine (fromRadians $ distanceBetween x origin_point_3d)) 0) x)
            (SolidTexture $ rgbColor (0.75,0.5,1.0))
            36
	    (points2d $ zipWith (,) [0.0,(0.3*pi)..(9*pi)] [0.0,0.0..])