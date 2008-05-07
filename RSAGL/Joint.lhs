\section{Joints}

Joints are a basic essential element of the RSAGL inverse kinematics subsystem.

\begin{code}
module RSAGL.Joint
    (Joint(..),
     joint) where

import RSAGL.Vector
import RSAGL.Affine
import RSAGL.Interpolation
import RSAGL.CoordinateSystems
import RSAGL.Orthagonal
\end{code}

\texttt{Joint} is the result of computing a joint.  It provides AffineTransformations that describe the orientations of the bases of the components of the joint.

\texttt{joint\_arm\_upper} is the affine transformation to the position of the upper arm, where the origin is the shoulder (or base).
\texttt{joint\_arm\_lower} is the affine transformation to the lower arm, where the origin is the elbow.
\texttt{joint\_arm\_hand} is the affine transformation where the origin is the hand.
\texttt{joint\_shoulder}, \texttt{joint\_hand}, and \texttt{joint\_elbow} refer to the positions of those endpoints of the joint.

\begin{code}
data Joint = Joint { joint_shoulder :: Point3D,
                     joint_hand :: Point3D,
                     joint_elbow :: Point3D,
                     joint_arm_lower :: AffineTransformation,
                     joint_arm_upper :: AffineTransformation,
		     joint_arm_hand :: AffineTransformation }
\end{code}

Compute a joint where given a bend vector, two end points, and the total length of them limb.  

\begin{code}
joint :: Vector3D -> Point3D -> Double -> Point3D -> Joint
joint bend shoulder joint_length hand | distanceBetween shoulder hand > joint_length = -- if the end is out of range, constrict it to within range
    joint bend shoulder joint_length (translate (vectorScaleTo (0.99 * joint_length) $ vectorToFrom hand shoulder) shoulder)
joint bend shoulder joint_length hand = Joint {
        joint_shoulder = shoulder,
        joint_hand = hand,
        joint_elbow = elbow,
        joint_arm_lower = modelLookAt elbow (forward $ Left hand) (down $ Right bend),
        joint_arm_upper = modelLookAt shoulder (forward $ Left elbow) (down $ Right bend),
	joint_arm_hand = modelLookAt hand (backward $ Left elbow) (up $ Right (Vector3D 0 1 0)) }
    where joint_offset = sqrt (joint_length^2 - (distanceBetween shoulder hand)^2) / 2
          joint_offset_vector = vectorScaleTo joint_offset $ transformation
              (orthagonalFrame (forward $ vectorToFrom hand shoulder) (down bend)) (Vector3D 0 (-1) 0)
          elbow = translate joint_offset_vector $ lerp 0.5 (shoulder,hand)
\end{code}
