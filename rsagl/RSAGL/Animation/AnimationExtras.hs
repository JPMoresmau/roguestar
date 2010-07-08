{-# LANGUAGE Arrows, TypeFamilies #-}

module RSAGL.Animation.AnimationExtras
    (rotationA,
     animateA,
     rotateA,
     pointAtCameraA,
     inverseSquareLaw,
     quadraticTrap,
     drag,
     concatForces,
     constrainForce,
     accelerationModel)
    where

import RSAGL.Math.Vector
import RSAGL.FRP
import RSAGL.Math.AbstractVector
import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Affine
import RSAGL.Math.Angle
import RSAGL.Scene.Scene
import RSAGL.Modeling.Model
import RSAGL.Scene.WrappedAffine
import Control.Monad
import RSAGL.Math.Types

-- | Answers a continuous rotation.
rotationA :: Vector3D -> Rate Angle -> FRP e m ignored AffineTransformation
rotationA v a = proc _ ->
    do t <- absoluteTime -< ()
       returnA -< rotate v (a `over` t)

-- | Apply an affine transformation to an animation.
animateA :: (CoordinateSystemClass s,StateOf m ~ s) => FRP e m j AffineTransformation -> FRP e m j p -> FRP e m j p
animateA affineA action = proc i ->
    do at <- affineA -< i
       transformA action -< (affineOf at,i)

-- | Animation that rotates continuously.
rotateA :: (CoordinateSystemClass s,StateOf m ~ s) => Vector3D -> Rate Angle -> FRP e m j p -> FRP e m j p
rotateA v a = animateA (rotationA v a)

-- | Always rotates the model so that it's Y+ axis points directly at the camera.
pointAtCameraA :: (Arrow a,ArrowState s a,CoordinateSystemClass s,ScenicAccumulator s m,ModelType model) => a (SceneLayer,m model) ()
pointAtCameraA = proc (slayer,the_model) ->
    do cs <- arr getCoordinateSystem <<< fetch -< ()
       accumulateSceneA -< (slayer,cameraRelativeSceneObject $ \c -> 
           liftM ((rotateToFrom (vectorToFrom (migrateToFrom root_coordinate_system cs $ camera_position c) $ origin_point_3d)
                                       (Vector3D 0 1 0)) . wrapAffine) (liftM toIntermediateModel the_model))

-- | A particle with a position and velocity.
type PV = (Point3D,Rate Vector3D)

-- | A particle with a position, velocity and acceleration.
type PVA = (Point3D,Rate Vector3D,Acceleration Vector3D)

-- | A time-varying, velocity-aware vector field that can act on a particle.
type ForceFunction = Time -> Point3D -> Rate Vector3D -> Acceleration Vector3D

-- | An energy-conserving force function describing gravitational attraction.
-- Accepts the intensity and singularity of the vector field.
inverseSquareLaw :: RSdouble -> Point3D -> ForceFunction
inverseSquareLaw g attractor _ p _ = perSecond $ perSecond $ vectorScaleTo (g * (recip $ vectorLengthSquared v)) v
    where v = vectorToFrom attractor p

-- | An energy-conserving force function that increases in
-- intensity with distance.
quadraticTrap :: RSdouble -> Point3D -> ForceFunction
quadraticTrap g attractor _ p _ = perSecond $ perSecond $ vectorScaleTo (g * vectorLengthSquared v) v
    where v = vectorToFrom attractor p

-- | A force function describing aerodynamic drag.
drag :: RSdouble -> ForceFunction
drag x _ _ v' = perSecond $ perSecond $ vectorScaleTo (negate $ x * vectorLengthSquared v) v
    where v = v' `over` fromSeconds 1

-- | Add many forces together.
concatForces :: [ForceFunction] -> ForceFunction
concatForces ffs t p v = abstractSum $ map (\f -> f t p v) ffs

-- | A filter function on forces.  Where the filter is False,
-- the force is coerced to zero.
constrainForce :: (Time -> Point3D -> Rate Vector3D -> Bool) -> ForceFunction -> ForceFunction
constrainForce predicate f t p v = if predicate t p v
    then f t p v
    else zero

-- | Apply a varying force function to a particle.
accelerationModel :: (CoordinateSystemClass s,StateOf m ~ s) =>
                     Frequency -> PV -> FRP e m j ForceFunction ->
                     FRP e m (PVA,j) p -> FRP e m j p
accelerationModel f pv forceA actionA = proc j ->
    do (p,v) <- integralRK4' f (flip translate) pv <<< forceA -< j
       a <- derivative -< v
       transformA actionA -< (affineOf $ translate (vectorToFrom p origin_point_3d),((p,v,a),j))

