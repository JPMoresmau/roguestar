{-# LANGUAGE TypeFamilies, Arrows #-}

module RSAGL.Animation.KinematicSensors
    (odometer,
     inertia)
    where

import Control.Arrow
import RSAGL.Math.Vector
import RSAGL.FRP
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math

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

-- | Measures the (presumed) acceleration due to inertia of a point
-- in the local coordinate system, relative to a (presumably) inertial frame a reference.
inertia :: (CoordinateSystemClass s,StateOf m ~ s) =>
           CoordinateSystem -> Point3D -> FRP e m () (Acceleration Vector3D)
inertia cs p = proc () -> arr (scalarMultiply (-1)) <<< derivative <<< derivative <<< exportToA cs -< p

