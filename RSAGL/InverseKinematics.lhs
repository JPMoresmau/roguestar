\section{Inverse Kinematics}

\begin{code}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

module RSAGL.InverseKinematics
    (leg,
     Leg,
     legAnimation,
     legs,
     approach,
     approachA)
    where

import Control.Arrow
import Control.Arrow.Operations
import Data.Fixed
import RSAGL.Vector
import RSAGL.FRP
import RSAGL.Affine
import RSAGL.CoordinateSystems
import RSAGL.KinematicSensors
import RSAGL.Vector
import RSAGL.Joint
import RSAGL.Time
import RSAGL.AbstractVector
import RSAGL.Angle
\end{code}

\subsection{The Foot}

This simulates a single foot that hops along by itself whenever its coordinate system moves.  A \texttt{foot} always trys to walk on the plane \texttt{y == 0}.
\texttt{foot} takes as input a boolean which, if true, indicates that there are not enough feet on the ground and that this \texttt{foot} should perform an
\"emergency foot down.\"  \texttt{foot} ignores the emergency foot down flag if it is already down.  \texttt{foot} emits the position of the foot and a boolean
which indicates if the foot is up (\texttt{True}) or down (\texttt{False}).

\texttt{foot} works by reading two odometers, one for forward movement and another for sideways movement.  This is like spinning the wheel by a magical odometer
instead of spinning the odometer by the wheel.

\texttt{pre_stepage} is the total odometer reading for all travel in both directions.  \texttt{stepage_adjustment} is the amount by which we need to increase
\texttt{pre_stepage} to ensure that the foot is correct based on an accumulation of emergency foot downs.  \texttt{cyclic_stepage} is a value between 0 and 2,
where a value greater than 1 indicates that the foot is down.

\begin{code}
foot :: (Arrow a,ArrowApply a,ArrowChoice a,CoordinateSystemClass s,ArrowState s a) => Double -> Double -> Double -> FRP i o a Bool (CSN Point3D,Bool)
foot forward_radius side_radius lift_radius = proc emergency_footdown ->
    do fwd_total_stepage <- arr (* recip forward_radius) <<< odometer root_coordinate_system (Vector3D 0 0 1) -< ()
       side_total_stepage <- arr (* recip side_radius) <<< odometer root_coordinate_system (Vector3D 1 0 0) -< ()
       let pre_stepage = sqrt $ fwd_total_stepage^2 + side_total_stepage^2
       stepage_adjustment <- integralRK4 fps30 add 0 -< (\_ p -> if (p + pre_stepage) `mod'` 2 < 1 && emergency_footdown then perSecond 1 else perSecond 0) 
       let adjusted_stepage = stepage_adjustment + pre_stepage
       let cyclic_stepage = (`mod'` 2) $ adjusted_stepage
       motion <- derivative -< adjusted_stepage
       let foot_lift = max 0 $ min 1 (motion `over` fromSeconds 1) * lift_radius * (sine $ fromRotations (cyclic_stepage / 2))
       let stepage_offset = if cyclic_stepage > 1 then 1.5 - cyclic_stepage else cyclic_stepage - 0.5
       let step_vector = scale (Vector3D side_radius 0 forward_radius) $ vectorScaleTo stepage_offset $ Vector3D side_total_stepage 0 fwd_total_stepage
       foot_position <- importA <<< arr (remoteCSN root_coordinate_system $ scale (Vector3D 1 0 1)) <<< exportA -< translate step_vector origin_point_3d
       csn_foot_position <- exportA -< translate (Vector3D 0 foot_lift 0) foot_position
       returnA -< (csn_foot_position,cyclic_stepage > 1)
\end{code}

\texttt{leg} creates a leg that tries to walk over the ground no matter how it is positioned.  \texttt{legs} combines a group of legs so that at least half
of the legs always try to touch the ground.

\begin{code}
newtype Leg i o a = Leg (FRP i o a [Bool] [Bool])

instance (ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => AffineTransformable (Leg i o a) where
    transform m (Leg l) = Leg (proc x -> transformA l -< (transform m,x))

leg :: (ArrowApply a,ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => Vector3D -> Point3D -> Double -> Point3D -> (FRP i o a Joint ()) -> Leg i o a
leg bend base len end animation = Leg $ proc feet_that_are_down ->
    do let declare_emergency_foot_down = length (filter id feet_that_are_down) < length (filter not feet_that_are_down) &&
                                         not (and $ take 1 feet_that_are_down)
       (p,foot_is_down) <- first importA <<< 
                           transformA (foot foot_radius foot_radius (foot_radius/5)) -< (translate (vectorToFrom end origin_point_3d),declare_emergency_foot_down)
       animation -< joint bend base len p
       returnA -< (foot_is_down || declare_emergency_foot_down) : feet_that_are_down
  where foot_radius = sqrt (len^2 - (distanceBetween base end)^2) / 2

legs :: (ArrowChoice a) => [Leg i o a] -> FRP i o a () ()
legs ls = (foldl (>>>) (arr $ const []) $ map (\(Leg l) -> l) ls) >>> (arr $ const ())
\end{code}

\texttt{legAnimation} is just a simple combinator to combine the upper and lower components of a leg into an animated Joint.

\begin{code}
legAnimation :: (ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => FRP i o a () () -> FRP i o a () () -> FRP i o a Joint ()
legAnimation upper_leg lower_leg = proc j ->
    do transformA upper_leg -< (joint_arm_upper j,())
       transformA lower_leg -< (joint_arm_lower j,())
\end{code}

\subsection{Approach}

\texttt{approach} is an acceleration function that that tries to approach a goal point.  It begins slowing down when it comes within
the goal radius, and otherwise travels at a fixed speed toward the goal.  The goal radius and speed are defined in terms of the
\texttt{magnitude} of the vector type.

\begin{code}
approach :: (AbstractVector v,AbstractSubtract p v,AbstractMagnitude v) => p -> Double -> Rate Double -> (Time -> p -> Rate v)
approach goal_point goal_radius max_speed _ position = withTime (fromSeconds 1) (\x -> abstractScaleTo (x * speed_ratio) goal_vector) max_speed
    where goal_vector = goal_point `sub` position
          speed_ratio = min 1 $ magnitude goal_vector / goal_radius

approachA :: (ArrowChoice a,ArrowApply a,AbstractVector v,AbstractAdd p v, AbstractSubtract p v,AbstractMagnitude v) => Double -> Rate Double -> FRPX any t i o a p p
approachA goal_radius max_speed = frp1Context $ proc initial_value -> switchContinue -< (Just $ approachA_ initial_value,initial_value)
    where approachA_ initial_value = proc goal_point -> integralRK4 frequency add initial_value -< approach goal_point goal_radius max_speed
          frequency = 8 `per` time goal_radius max_speed 
\end{code}

