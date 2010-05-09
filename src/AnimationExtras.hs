{-# LANGUAGE Arrows,
             OverloadedStrings,
             TypeFamilies,
             Rank2Types,
             FlexibleContexts #-}

module AnimationExtras
    (genericStateHeader,
     floatBobbing,
     newListElements,
     basic_camera)
    where

import Animation
import RSAGL.Math
import RSAGL.FRP
import RSAGL.Scene
import Control.Arrow
import RSAGL.Types
import Data.List ((\\))
import qualified Data.ByteString.Char8 as B

-- | Switch out if the driver \"state\" does match the specified predicate.
genericStateHeader :: (FRPModel m, StateOf m ~ AnimationState) => 
                      (B.ByteString -> FRP e m (SwitchInputOf m) (SwitchOutputOf m)) -> 
                      (B.ByteString -> Bool) -> 
                      FRP e m (SwitchInputOf m) ()
genericStateHeader switchTo f = proc i ->
    do m_state <- driverGetAnswerA -< "state"
       switchContinue -< (if fmap f m_state == Just True then Nothing else fmap switchTo m_state,i)
       returnA -< ()

-- | Animate something bobbing up and down.
floatBobbing :: (FRPModel m,StateOf m ~ AnimationState) => RSdouble -> RSdouble -> FRP e m j p -> FRP e m j p
floatBobbing ay by animationA = proc j ->
    do t <- threadTime -< ()
       let float_y = lerpBetween (-1,sine $ fromRotations $ t `cyclical'` (fromSeconds 5),1) (ay,by)
       transformA animationA -< (Affine $ translate (Vector3D 0 float_y 0),j)

-- | Get new elements in a list on a frame-by-frame basis.
newListElements :: (FRPModel m,Eq a) => FRP e m [a] [a]
newListElements = proc as ->
    do olds_as <- delay [] -< as
       returnA -< as \\ olds_as

-- | A simple default forward-looking camera.
basic_camera :: Camera
basic_camera = PerspectiveCamera {
    camera_position = Point3D 0 0 0,
    camera_lookat = Point3D 0 0 1,
    camera_up = Vector3D 0 1 0,
    camera_fov = fromDegrees 45 }

