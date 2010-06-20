{-# LANGUAGE Arrows,
             ExistentialQuantification,
             UndecidableInstances,
             TypeFamilies #-}

module RSAGL.Animation.InverseKinematics
    (leg,
     Leg,
     jointAnimation,
     legs,
     approach,
     approachFrom,
     approachA)
    where

import Control.Arrow
import RSAGL.Math.Vector
import RSAGL.FRP
import RSAGL.Math.Affine
import RSAGL.Scene.CoordinateSystems
import RSAGL.Animation.KinematicSensors
import RSAGL.Animation.Joint
import RSAGL.Math.AbstractVector
import RSAGL.Math.Angle
import RSAGL.Math.FMod
import RSAGL.Types

-- | This simulates a single foot that hops along by itself whenever its
-- coordinate system moves.  A foot always trys to walk on the plane @y == 0@.
-- foot takes as input a boolean which, if true, indicates that there are not
-- enough feet on the ground and that this \texttt{foot} should perform an
-- \"emergency foot down.\"  'foot' ignores the emergency foot down flag if it
-- is already down.  'foot' emits the position of the foot and a boolean
-- which indicates if the foot is up (True) or down (False).

-- foot works by reading two odometers, one for forward movement and another for
-- sideways movement.  This is like spinning the wheel by a magical odometer
-- instead of spinning the odometer by the wheel.
--
-- 'pre_stepage' is the total odometer reading for all travel in both
-- directions.  'stepage_adjustment' is the amount by which we need to increase
-- 'pre_stepage' to ensure that the foot is correct based on an accumulation of
-- emergency foot downs.  'cyclic_stepage' is a value between 0 and 2,
-- where a value greater than 1 indicates that the foot is down.
foot :: (CoordinateSystemClass s,s ~ StateOf m) =>
        RSdouble ->
        RSdouble ->
        RSdouble ->
        FRP e m Bool (CSN Point3D,Bool)
foot forward_radius side_radius lift_radius = proc emergency_footdown ->
       -- total forward travel of the foot:
    do fwd_total_stepage <- arr (* recip forward_radius) <<<
           odometer root_coordinate_system (Vector3D 0 0 1) -< ()
       -- total sideways travel of the foot
       side_total_stepage <- arr (* recip side_radius) <<<
           odometer root_coordinate_system (Vector3D 1 0 0) -< ()
       -- total travel of the foot
       let pre_stepage = sqrt $ fwd_total_stepage^2 + side_total_stepage^2
       -- adjustment to comply with \"emergency foot downs.\"
       stepage_adjustment <- integralRK4 fps30 add 0 -<
               (\_ p -> if (p + pre_stepage) `fmod` 2 < 1 && emergency_footdown
                           then perSecond 1
                           else perSecond 0)
       let adjusted_stepage = stepage_adjustment + pre_stepage
       -- a value between 0 and 2, where a value greater than 1 indicates that
       -- the foot is down
       let cyclic_stepage = (`fmod` 2) $ adjusted_stepage
       motion <- derivative -< adjusted_stepage
       let foot_lift = max 0 $ min 1 (motion `over` fromSeconds 1) *
                               lift_radius *
                               (sine $ fromRotations (cyclic_stepage / 2))
       let stepage_offset = if cyclic_stepage > 1
                                then 1.5 - cyclic_stepage
                                else cyclic_stepage - 0.5
       let step_vector = scale (Vector3D side_radius 0 forward_radius) $
                             vectorScaleTo stepage_offset $
                                 Vector3D side_total_stepage 0 fwd_total_stepage
       foot_position <- importA <<<
           arr (remoteCSN root_coordinate_system $ scale (Vector3D 1 0 1)) <<<
               exportA -< translate step_vector origin_point_3d
       csn_foot_position <- exportA -<
           translate (Vector3D 0 foot_lift 0) foot_position
       returnA -< (csn_foot_position,cyclic_stepage > 1)

-- | A description of a leg.
data Leg e m = Leg (FRP e m [Bool] [Bool])

instance (CoordinateSystemClass s,StateOf m ~ s) =>
         AffineTransformable (Leg e m) where
    transform m (Leg l) =
        Leg (proc x -> transformA l -< (Affine $ transform m,x))

-- | Constructs a leg, based on several parameters.
-- See the descriptions of 'foot' and 'joint' for
-- more information on how some of these parameters
-- are interpreted.
--
-- * bend - The bend vector of the articulated joint
-- * base - the base or shoulder or fixed point of the joint
-- * len - the total length of the articulation
-- * end - the natural resting point of the foot
--         the foot will rarely actually be here,
--         but will try to return to this point
--         as it walks.
-- * animation - an animation to display the joint
leg :: (CoordinateSystemClass s,StateOf m ~ s) =>
       Vector3D ->
       Point3D ->
       RSdouble ->
       Point3D ->
       (FRP e m Joint ()) ->
       Leg e m
leg bend base len end animation = Leg $ proc feet_that_are_down ->
    do let declare_emergency_foot_down =
               length (filter id feet_that_are_down) <
               length (filter not feet_that_are_down) &&
               not (and $ take 1 feet_that_are_down)
       (p,foot_is_down) <- first importA <<<
           transformA (foot foot_radius foot_radius (foot_radius/5)) -<
               (Affine $ translate (vectorToFrom end origin_point_3d),
                declare_emergency_foot_down)
       animation -< joint bend base len p
       returnA -< (foot_is_down || declare_emergency_foot_down) :
                  feet_that_are_down
  where foot_radius = sqrt (len^2 - (distanceBetween base end)^2) / 2

-- | Combines a group of legs into a group that will try to keep at least half
-- of the legs on the ground at all times.
legs :: [Leg e m] -> FRP e m () ()
legs ls = (foldl (>>>) (arr $ const []) $ map (\(Leg l) -> l) ls) >>>
          (arr $ const ())

-- | Animates the upper and lower limbs of a joint into a single animation,
-- using the affine transformations 'joitn_arm_upper' and 'joint_arm_lower'.
jointAnimation :: (CoordinateSystemClass s,StateOf m ~ s) =>
                  FRP e m () () ->
                  FRP e m () () ->
                  FRP e m Joint ()
jointAnimation upperJoint lowerJoint = proc j ->
    do transformA upperJoint -< (affineOf $ joint_arm_upper j,())
       transformA lowerJoint -< (affineOf $ joint_arm_lower j,())

-- | An acceleration function that that tries to approach a goal point.
-- It begins slowing down when it comes within the goal radius, and otherwise
-- travels at a fixed speed toward the goal.  The goal radius and speed are
-- defined in terms of the magnitude method of the vector type.
--
-- This function is just a pure differential equation, see 'approachFrom'
-- and 'approachA'.
--
-- Parameters are:
-- * A goal point.
-- * A goal radius.
-- * An approach speed.
--
approach :: (AbstractVector v,AbstractSubtract p v,AbstractMagnitude v) =>
            p ->
            RSdouble ->
            Rate RSdouble ->
            (Time -> p -> Rate v)
approach goal_point goal_radius max_speed _ position =
              withTime (fromSeconds 1)
                       (\x -> abstractScaleTo (x * speed_ratio) goal_vector)
                       max_speed
    where goal_vector = goal_point `sub` position
          speed_ratio = min 1 $ magnitude goal_vector / goal_radius

-- | Approach a moving goal point from the specified starting point.
-- See the description of 'approach'.
approachFrom :: (AbstractVector v,
                 AbstractAdd p v,
                 AbstractSubtract p v,
                 AbstractMagnitude v) =>
                RSdouble ->
                Rate RSdouble ->
                p ->
                FRP e m p p
approachFrom goal_radius max_speed initial_value = proc goal_point ->
        integralRK4 frequency add initial_value -<
            approach goal_point goal_radius max_speed
    where frequency = 1 `per` time goal_radius max_speed

-- | Approach a moving goal point, starting at the initial
-- position of the goal point.  The particle won't move
-- until the goal point moves.
approachA :: (AbstractVector v,
              AbstractAdd p v,
              AbstractSubtract p v,
              AbstractMagnitude v,
              FRPModel m) => RSdouble -> Rate RSdouble -> FRP e m p p
approachA goal_radius max_speed = frp1Context $ proc p -> switchContinue -<
    (Just $ approachFrom goal_radius max_speed p,p)

