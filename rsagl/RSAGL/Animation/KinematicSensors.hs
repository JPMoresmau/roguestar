{-# LANGUAGE TypeFamilies #-}

module RSAGL.Animation.KinematicSensors
    (odometer)
    where

import Control.Arrow
import RSAGL.Math.Vector
import RSAGL.FRP
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Types

-- | Measures the distance traveled, by the origin of the local coordinate
-- system, as measured in a remote coordinate system, in terms
-- of a vector in the local coordinate system.
--
-- For example, if we are animating a model of a wheel, we could put an
-- odometer at the bottom-most point of the wheel, and then rotate
-- the wheel according to the result.  The wheel would seem to
-- turn as it travels, but would not turn when dragged sidewise.
odometer :: (CoordinateSystemClass s,StateOf m ~ s) => CoordinateSystem -> Vector3D -> FRP e m () RSdouble
odometer cs measurement_vector_ =
       arr (const origin_point_3d) >>> exportToA cs >>> derivative >>> importFromA cs >>>
       arr (withTime (fromSeconds 1) (dotProduct measurement_vector)) >>>
       integral 0
    where measurement_vector = vectorNormalize measurement_vector_

