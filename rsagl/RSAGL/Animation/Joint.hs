module RSAGL.Animation.Joint
    (Joint(..),
     joint) where

import RSAGL.Math.Vector
import RSAGL.Math.Affine
import RSAGL.Math.Interpolation
import RSAGL.Scene.CoordinateSystems
import RSAGL.Math.Orthogonal
import RSAGL.Math.Types

-- | The result of computing a joint.  It provides AffineTransformations that
-- describe the orientations of the components of the joint.
-- All affine transformations reorient the +Z axis to aim in the direction
-- of the far point.  For example, in @joint_arm_lower@ the +Z axis aims
-- at the position of the hand.
data Joint = Joint { joint_shoulder :: Point3D,
                     -- ^ The fixed point of the joint.
                     joint_hand :: Point3D,
                     -- ^ The far end point of the joint.
                     joint_elbow :: Point3D,
                     -- ^ The articulated point of the joint.
                     joint_arm_lower :: AffineTransformation,
                     -- ^ The affine transformation to the lower
                     -- arm, where the origin is the elbow.
                     joint_arm_upper :: AffineTransformation,
                     -- ^ The affine transformation to the upper
                     -- arm, where the origin is the shoulder.
                     joint_arm_hand :: AffineTransformation
                     -- ^ The affine transformation where the origin
                     -- is the hand.  Oriented to preserve as much as
                     -- possible the +Y axis.
                     }

-- | Compute a joint where given a bend vector, describing the direction
-- in which the articulated point (elbow) will try to move when the
-- arm is retracted, and shoulder or base of the joint, the total
-- length of the joint, and ideal position of the hand.
joint :: Vector3D -> Point3D -> RSdouble -> Point3D -> Joint
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
              (orthogonalFrame (forward $ vectorToFrom hand shoulder) (down bend)) (Vector3D 0 (-1) 0)
          elbow = translate joint_offset_vector $ lerp 0.5 (shoulder,hand)

