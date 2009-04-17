{-# LANGUAGE Arrows #-}

module AnimationExtras
    (genericStateHeader,
     floatBobbing,
     basic_camera)
    where

import Animation
import RSAGL.Math
import RSAGL.FRP
import RSAGL.Scene
import Control.Arrow

-- | Switch out if the driver \"state\" does match the specified predicate.
genericStateHeader :: (String -> RSAnimA1 i o i o) -> (String -> Bool) -> RSAnimA1 i o i ()
genericStateHeader switchTo f = proc i ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (if fmap f m_state == Just True then Nothing else fmap switchTo m_state,i)
       returnA -< ()

-- | Animate something bobbing up and down.
floatBobbing :: Double -> Double -> RSAnimAX any t i o j p -> RSAnimAX any t i o j p
floatBobbing ay by animationA = proc j ->
    do t <- threadTime -< ()
       let float_y = lerpBetween (-1,sine $ fromRotations $ t `cyclical'` (fromSeconds 5),1) (ay,by)
       transformA animationA -< (Affine $ translate (Vector3D 0 float_y 0),j)

-- | A simple default forward-looking camera.
basic_camera :: Camera
basic_camera = PerspectiveCamera {
    camera_position = Point3D 0 0 0,
    camera_lookat = Point3D 0 0 1,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 45 }
