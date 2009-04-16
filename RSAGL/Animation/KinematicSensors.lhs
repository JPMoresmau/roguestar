\section{Sensors for Inverse Kinematics}

These are various composable modules that provide physics information for the inverse kinematics system.

\begin{code}
module RSAGL.Animation.KinematicSensors
    (odometer)
    where

import Control.Arrow
import Control.Arrow.Operations
import RSAGL.Math.Vector
import RSAGL.FRP.FRP
import RSAGL.Scene.CoordinateSystems
import RSAGL.FRP.Time
\end{code}

\subsection{Odometer}

The \texttt{odometer} indicates the distance traveled in a remote coordinate system along a vector in the local coordinate system.
It does not measure side-to-side motion, only motion in the direction of the vector.

\begin{code}
odometer :: (Arrow a,ArrowChoice a,ArrowApply a,ArrowState s a,CoordinateSystemClass s) => CoordinateSystem -> Vector3D -> FRPX any t i o a () Double
odometer cs measurement_vector_ =
       arr (const origin_point_3d) >>> exportToA cs >>> derivative >>> importFromA cs >>>
       arr (withTime (fromSeconds 1) (dotProduct measurement_vector)) >>>
       integral 0
    where measurement_vector = vectorNormalize measurement_vector_
\end{code}

