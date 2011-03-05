{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

module Limbs
    (bothEyeStalks,
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
import RSAGL.FRP
import EventUtils

-- | Animate an arbitrary articulated joint.
-- This scales the pieces of the joint, so that we can re-use the same model for
-- differently-lengthed appendages.
libraryJointAnimation :: (FRPModel m, FRPModes m ~ RoguestarModes) =>
                         RSdouble -> LibraryModel ->
                         LibraryModel -> FRP e m Joint ()
libraryJointAnimation maximum_length upper lower = proc joint_info ->
    jointAnimation (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(std_scene_layer_local,upper)))
                   (proc () -> transformA libraryA -< (Affine $ scale' (maximum_length/2),(std_scene_layer_local,lower))) -< 
		       joint_info

-- | Animate an articulated joint holding constant the bend vector and arm length.
arm :: (FRPModel m, FRPModes m ~ RoguestarModes) =>
       LibraryModel -> LibraryModel -> Vector3D -> RSdouble ->
       FRP e m (Point3D,Point3D) Joint
arm arm_upper arm_lower bend_vector maximum_length = proc (shoulder_point,hand_point) ->
    do let joint_info = joint bend_vector shoulder_point maximum_length hand_point
       libraryJointAnimation maximum_length arm_upper arm_lower -< joint_info
       returnA -< joint_info

eyeStalk :: (FRPModel m, FRPModes m ~ RoguestarModes) =>
       LibraryModel -> LibraryModel -> LibraryModel -> Vector3D -> Point3D -> RSdouble -> Point3D ->
       FRP e m () ()
eyeStalk stalk_upper stalk_lower eyeball bend_vector root_point maximum_length eyeball_point = proc () ->
    do a <- inertia root_coordinate_system eyeball_point -< ()
       (googly_eye,_,_) <- singleParticle fps120 (eyeball_point, zero) -< concatForces [
           \_ _ _ -> a,
           drag 20,
           quadraticTrap ((*2000) $ recip $ distanceBetween eyeball_point $ swapX eyeball_point) eyeball_point]
       let joint_info = joint bend_vector root_point maximum_length googly_eye
       libraryJointAnimation maximum_length stalk_upper stalk_lower -< joint_info
       transformA libraryA -< (Affine $ transformation $ joint_arm_hand joint_info,
                               (std_scene_layer_local,eyeball))
       returnA -< ()

bothEyeStalks :: (FRPModel m, FRPModes m ~ RoguestarModes) =>
       LibraryModel -> LibraryModel -> LibraryModel -> Vector3D -> Point3D -> RSdouble -> Point3D ->
       FRP e m () ()
bothEyeStalks stalk_upper stalk_lower eyeball bend_vector root_point maximum_length eyeball_point = proc () ->
    do eyeStalk stalk_upper stalk_lower eyeball
                bend_vector root_point maximum_length eyeball_point -< ()
       eyeStalk stalk_upper stalk_lower eyeball
                (swapX bend_vector)
                (swapX root_point)
                maximum_length
                (swapX eyeball_point) -< ()

-- | Animate a right arm.  This animation is aware of what tool the current
-- creature (based on thread ID) is holding and raises the arm forward
-- while holding any tool.
rightArm :: (FRPModel m, FRPModes m ~ RoguestarModes,
             ThreadIDOf m ~ Maybe Integer) =>
            LibraryModel -> LibraryModel ->
            Vector3D -> Point3D -> RSdouble -> Point3D ->
            FRP e m () Joint
rightArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = proc () ->
    do m_time_recent_attack <- recentAttack ThisObject -< ()
       t_now <- threadTime -< ()
       m_tool_type <- objectDetailsLookup (WieldedTool ThisObject) "tool-type" -< () 
       is_wielding <- isWielding ThisObject -< ()
       hand_point <- approachA 0.1 (perSecond 1.0) -< case m_time_recent_attack of
           Just t  | t_now < t `add` fromSeconds 0.5 && m_tool_type == Just "sword" -> translate (Vector3D maximum_length 0 0) shoulder_anchor
           Just t  | t_now < t `add` fromSeconds 0.3 && m_tool_type == Nothing -> translate (Vector3D 0 0 $ maximum_length / 4) shoulder_anchor
           Just t  | t_now < t `add` fromSeconds 1.0 && m_tool_type == Nothing -> translate (Vector3D 0 0 maximum_length) shoulder_anchor
           _       | is_wielding -> translate (Vector3D 0 0 maximum_length) shoulder_anchor
	   _       | otherwise   -> hand_rest
       arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_point)

-- | Animate a left arm, which is always held at the side.
leftArm :: (FRPModel m, FRPModes m ~ RoguestarModes,
            ThreadIDOf m ~ Maybe Integer) =>
           LibraryModel -> LibraryModel ->
           Vector3D -> Point3D -> RSdouble -> Point3D ->
           FRP e m () Joint
leftArm arm_upper arm_lower bend_vector shoulder_anchor maximum_length hand_rest = 
    proc () -> arm arm_upper arm_lower bend_vector maximum_length -< (shoulder_anchor,hand_rest)

-- | Animate two arms.  The parameters describe the right arm, and are swapped
-- across the yz plane to produce left arm parameters.
bothArms :: (FRPModel m,
             FRPModes m ~ RoguestarModes,
             ThreadIDOf m ~ Maybe Integer,
             LibraryModelSource lm1,
             LibraryModelSource lm2) =>
     lm1 ->
     lm2 ->
     Vector3D ->
     Point3D ->
     RSdouble ->
     Point3D ->
     FRP e m () (Joint,Joint)
bothArms arm_upper
         arm_lower
         bend_vector
         shoulder_anchor
         maximum_length
         hand_rest = proc () ->
    do left_joint <- leftArm (toLibraryModel arm_upper)
                             (toLibraryModel arm_lower)
                             (swapX bend_vector)
                             (swapX shoulder_anchor)
                             maximum_length
                             (swapX hand_rest) -< ()
       right_joint <- rightArm (toLibraryModel arm_upper)
                               (toLibraryModel arm_lower)
                               bend_vector
                               shoulder_anchor
                               maximum_length
                               hand_rest -< ()
       returnA -< (left_joint,right_joint)

-- | Animate legs, which automatically know how to take steps when moved.
bothLegs :: (FRPModel m,
             FRPModes m ~ RoguestarModes,
             LibraryModelSource lm1,
             LibraryModelSource lm2) =>
    lm1 ->
    lm2 ->
    Vector3D ->
    Point3D ->
    RSdouble ->
    Point3D ->
    FRP e m () ()
bothLegs leg_upper
         leg_lower
         bend_vector
         hip_anchor
         maximum_length
         foot_rest = proc () ->
    do legs [leg bend_vector
                 hip_anchor
                 maximum_length
                 foot_rest
                 (libraryJointAnimation maximum_length
                                        (toLibraryModel leg_upper)
                                        (toLibraryModel leg_lower)),
             leg (swapX bend_vector)
                 (swapX hip_anchor)
                 maximum_length
                 (swapX foot_rest)
                 (libraryJointAnimation maximum_length
                                        (toLibraryModel leg_upper)
                                        (toLibraryModel leg_lower))]
                 -< ()

swapX :: (AffineTransformable a) => a -> a
swapX = scale (Vector3D (-1.0) 1.0 1.0)
