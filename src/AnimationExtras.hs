{-# LANGUAGE Arrows #-}

module AnimationExtras
    (floatBobbing)
    where

import Animation
import RSAGL.Interpolation
import RSAGL.Time
import RSAGL.Angle
import RSAGL.Affine
import RSAGL.FRP
import RSAGL.CoordinateSystems
import RSAGL.Vector

-- | Animate something bobbing up and down.
floatBobbing :: Double -> Double -> RSAnimAX any t i o j p -> RSAnimAX any t i o j p
floatBobbing ay by animationA = proc j ->
    do t <- threadTime -< ()
       let float_y = lerpBetween (-1,sine $ fromRotations $ t `cyclical'` (fromSeconds 5),1) (ay,by)
       transformA animationA -< (Affine $ translate (Vector3D 0 float_y 0),j)

