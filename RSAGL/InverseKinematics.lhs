\section{Inverse Kinematics}

\begin{code}
{-# LANGUAGE Arrows #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

module RSAGL.InverseKinematics
    (leg,
     Leg,
     legAnimation,
     legs)
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
\end{code}

\subsection{The Foot}

This simulates a single foot that hops along by itself whenever its coordinate system moves.  A \texttt{foot} always trys to walk on the plane \texttt{y == 0}.

\begin{code}
foot :: (Arrow a,ArrowApply a,ArrowChoice a,CoordinateSystemClass s,ArrowState s a) => Double -> Double -> FRP i o a Double (CSN Point3D)
foot forward_radius side_radius = proc stepage_offset ->
    do fwd_total_stepage <- arr (* recip forward_radius) <<< odometer root_coordinate_system (Vector3D 0 0 1) -< ()
       side_total_stepage <- arr (* recip side_radius) <<< odometer root_coordinate_system (Vector3D 1 0 0) -< ()
       let stepage = stepageF $ (`mod'` 2) $ (+ stepage_offset) $ sqrt $ fwd_total_stepage^2 + side_total_stepage^2
       let step_vector = scale (Vector3D side_radius 0 forward_radius) $ vectorScaleTo stepage $ Vector3D side_total_stepage 0 fwd_total_stepage
       arr (remoteCSN root_coordinate_system $ scale (Vector3D 1 0 1)) <<< exportA -< translate step_vector origin_point_3d
  where stepageF u = if u > 1 then 1.5 - u else u - 0.5
\end{code}

\texttt{leg} creates a leg that tries to walk over the ground no matter how it is positioned.  \texttt{legs} combines a group of legs so that at least half
of the legs always try to touch the ground.

\begin{code}
newtype Leg i o a = Leg (FRP i o a () ())

instance (ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => AffineTransformable (Leg i o a) where
    transform m (Leg l) = Leg (proc x -> transformA l -< (transform m,x))

leg :: (ArrowApply a,ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => Vector3D -> Point3D -> Double -> Point3D -> (FRP i o a Joint ()) -> Leg i o a
leg bend base len end animation = Leg $ proc () ->
    do p <- importA <<< transformA (foot foot_radius foot_radius) -< (translate (vectorToFrom end origin_point_3d),0.0)
       animation -< joint bend base len p
  where foot_radius = sqrt (len^2 - (distanceBetween base end)^2) / 2

legs :: (ArrowChoice a) => [Leg i o a] -> FRP i o a () ()
legs = foldr (>>>) (arr $ const ()) . map (\(Leg l) -> l)
\end{code}

\texttt{legAnimation} is just a simple combinator to combine the upper and lower components of a leg into an animated Joint.

\begin{code}
legAnimation :: (ArrowChoice a,ArrowState s a,CoordinateSystemClass s) => FRP i o a () () -> FRP i o a () () -> FRP i o a Joint ()
legAnimation upper_leg lower_leg = proc j ->
    do transformA upper_leg -< (joint_arm_upper j,())
       transformA lower_leg -< (joint_arm_lower j,())
\end{code}