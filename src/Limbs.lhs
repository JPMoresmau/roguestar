\section{Limbs}

\begin{code}
{-# LANGUAGE Arrows #-}

module Limbs
    (rightArm,
     leftArm,
     bothArms,
     bothLegs)
    where
import VisibleObject
import Animation
import RSAGL.Animation
import RSAGL.Joint
import RSAGL.Affine
import Models.Library
import Models.LibraryData
import Control.Arrow
import RSAGL.Vector
import RSAGL.CoordinateSystems
import RSAGL.Scene
import RSAGL.InverseKinematics
import Data.Maybe
import RSAGL.Time

libraryJointAnimation :: Double -> LibraryModel -> LibraryModel -> RSAnimAX any t i o Joint ()
libraryJointAnimation maximum_length upper lower = proc joint_info ->
    jointAnimation (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(Local,upper)))
                   (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(Local,lower))) -< joint_info

arm :: LibraryModel -> LibraryModel -> Vector3D -> Double -> RSAnimAX any t i o (Point3D,Point3D) Joint
arm arm_upper arm_lower bend_vector maximum_length = proc (shoulder_point,hand_point) ->
    do let joint_info = joint bend_vector shoulder_point maximum_length hand_point
       libraryJointAnimation maximum_length arm_upper arm_lower -< joint_info 
       returnA -< joint_info

rightArm :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimA (Maybe Integer) i o () Joint
rightArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = proc () ->
    do is_wielding <- arr isJust <<< wieldedTool -< ()
       hand_point <- approachA 0.1 (perSecond 1.0) -< if is_wielding
           then translate (Vector3D 0 0 maximum_length) shoulder_anchor
	   else hand_rest
       arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_point)

leftArm :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimA (Maybe Integer) i o () Joint
leftArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = 
    proc () -> arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_rest)
\end{code}

\texttt{bothArms} and \texttt{bothLegs} render two limbs by swapping the description for the right limb.

\begin{code}
bothArms :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimA (Maybe Integer) i o () (Joint,Joint)
bothArms arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = proc () ->
    do left_joint <- leftArm arm_upper arm_lower (swapX bend_vector) (swapX shoulder_anchor) maximum_length (swapX hand_rest) -< ()
       right_joint <- rightArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest -< ()
       returnA -< (left_joint,right_joint)

bothLegs :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimA t i o () ()
bothLegs leg_upper leg_lower bend_vector hip_anchor maximum_length foot_rest = proc () ->
    do legs [leg bend_vector hip_anchor maximum_length foot_rest (libraryJointAnimation maximum_length leg_upper leg_lower),
             leg bend_vector (swapX hip_anchor) maximum_length (swapX foot_rest) (libraryJointAnimation maximum_length leg_upper leg_lower)]
	         -< ()

swapX :: (AffineTransformable a) => a -> a
swapX = scale (Vector3D (-1.0) 1.0 1.0)
\end{code}
