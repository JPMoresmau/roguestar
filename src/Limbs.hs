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
import RSAGL.Math
import Models.LibraryData
import Control.Arrow
import RSAGL.Scene
import Data.Maybe
import RSAGL.FRP

-- | Animate an arbitrary articulated joint.
libraryJointAnimation :: Double -> LibraryModel -> LibraryModel -> RSAnimAX k t i o Joint ()
libraryJointAnimation maximum_length upper lower = proc joint_info ->
    jointAnimation (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(std_scene_layer_local,upper)))
                   (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(std_scene_layer_local,lower))) -< 
		       joint_info

-- | Animate an articulated joint holding constant the bend vector and arm length.
arm :: LibraryModel -> LibraryModel -> Vector3D -> Double -> RSAnimAX k t i o (Point3D,Point3D) Joint
arm arm_upper arm_lower bend_vector maximum_length = proc (shoulder_point,hand_point) ->
    do let joint_info = joint bend_vector shoulder_point maximum_length hand_point
       libraryJointAnimation maximum_length arm_upper arm_lower -< joint_info 
       returnA -< joint_info

-- | Animate a right arm.  This animation is aware of what tool the current creature (based on thread ID) is holding and raises the arm forward
-- while holding any tool.
rightArm :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimAX Threaded (Maybe Integer) i o () Joint
rightArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = proc () ->
    do is_wielding <- isWielding ThisObject -< ()
       hand_point <- approachA 0.1 (perSecond 1.0) -< if is_wielding
           then translate (Vector3D 0 0 maximum_length) shoulder_anchor
	   else hand_rest
       arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_point)

-- | Animate a left arm, which is always held at the side.
leftArm :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimAX Threaded (Maybe Integer) i o () Joint
leftArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = 
    proc () -> arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_rest)

-- | Animate two arms.  The parameters describe the right arm, and are swapped across the yz plane to produce left arm parameters.
bothArms :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimAX Threaded (Maybe Integer) i o () (Joint,Joint)
bothArms arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = proc () ->
    do left_joint <- leftArm arm_upper arm_lower (swapX bend_vector) (swapX shoulder_anchor) maximum_length (swapX hand_rest) -< ()
       right_joint <- rightArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest -< ()
       returnA -< (left_joint,right_joint)

-- | Animate legs, which automatically know how to take steps when moved.
bothLegs :: LibraryModel -> LibraryModel -> Vector3D -> Point3D -> Double -> Point3D -> RSAnimAX Threaded t i o () ()
bothLegs leg_upper leg_lower bend_vector hip_anchor maximum_length foot_rest = proc () ->
    do legs [leg bend_vector hip_anchor maximum_length foot_rest (libraryJointAnimation maximum_length leg_upper leg_lower),
             leg bend_vector (swapX hip_anchor) maximum_length (swapX foot_rest) (libraryJointAnimation maximum_length leg_upper leg_lower)]
	         -< ()

swapX :: (AffineTransformable a) => a -> a
swapX = scale (Vector3D (-1.0) 1.0 1.0)
