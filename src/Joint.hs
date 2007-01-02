module Joint
    (Joint(..),
     JointParams(..),
     leftSide,
     joint) where
    
import Math
import Math3D
import Quality
import Model

-- |
-- A result from using the joint function.
-- [@joint@] is the position of the joint's elbow.
-- [@joint_hand_vector@] is the transformation matrix that would translate a model of a hand into
--     the correct position.  That is, the point (0,0,0) would be translated to joint_end, and
--     the vector (0,0,1) would be transformed to the vector from joint to joint_end.
--
data Joint = Joint { joint_base :: Point3D,
                     joint_end :: Point3D,
                     joint_bend :: Point3D,
                     joint_hand :: Matrix Float,
                     joint_arm :: Matrix Float,
                     joint_shoulder :: Matrix Float }
                       deriving (Show)

data JointParams = JointParams { joint_params_base :: Point3D,
                                 joint_params_end :: Point3D,
                                 joint_params_length :: Float,
                                 joint_params_bend_vector :: Vector3D }
                                   deriving (Show)

instance AffineTransformable JointParams where
    transform mat j = j { joint_params_base = base_result,
                          joint_params_end = end_result,
                          joint_params_length = distanceBetween base_result end_result /
                                                distanceBetween (joint_params_base j) (joint_params_end j) *
                                                joint_params_length j,
                          joint_params_bend_vector = transform mat $ joint_params_bend_vector j }
                            where base_result = transform mat $ joint_params_base j
                                  end_result = transform mat $ joint_params_end j

leftSide :: JointParams -> JointParams
leftSide = scale (Vector3D (-1) 1 1)

joint :: JointParams -> Joint
joint jps | distanceBetween (joint_params_base jps) (joint_params_end jps) >= joint_params_length jps =
    error $ "joint's length is less than the distance between it's base and end: " ++ show jps
joint jps = joint_ (joint_params_base jps) 
                   (joint_params_end jps) 
                   (joint_params_length jps)
                   (joint_params_bend_vector jps)

-- |
-- Calculate the shape of an articulated arm, given the arm's length,
-- a bend vector, and a base and endpoint of the entire arm.
-- The bend vector is the direction in which the join goes away from the line from
-- the base of the arm to the end of the arm.  For example, a human leg's bend
-- vector would point up and forward, while a human arm's bend vector would point
-- down and backwards.  Of course, in real life, it's easy to twist one's arm to give 
-- it a new bend vector.
--
joint_ :: Point3D -> Point3D -> Float -> Vector3D -> Joint
joint_ base end len bend_vector = 
    let joint_offset = sqrt $ len^2 - (distanceBetween base end)^2
        axis_vector = vectorNormalize $ vectorToFrom end base
        x_vector = crossProduct axis_vector (vectorNormalize bend_vector)
        y_vector = crossProduct axis_vector x_vector
        parametric_circle theta = vectorScale joint_offset $ vectorAdd (vectorScale (cos theta) x_vector) (vectorScale (sin theta) y_vector)
        joint_bend_result = translate (parametric_circle $ minimize (distanceBetween bend_vector . parametric_circle) (0,2*pi) 0.01) (lerp (0.5 :: Float) (base,end))
        in Joint { joint_base = base,
                   joint_end = end,
                   joint_bend = joint_bend_result,
                   joint_hand =     translate (vectorToFrom end origin_point_3d)               $ jointMatrix joint_bend_result end bend_vector,
                   joint_arm =      translate (vectorToFrom joint_bend_result origin_point_3d) $ jointMatrix joint_bend_result end bend_vector,
                   joint_shoulder = translate (vectorToFrom base origin_point_3d)              $ jointMatrix base joint_bend_result bend_vector }
                   
-- |
-- A rotation matrix so that the transformed object's +Z vector points from base to end, and it's Y+ vector
-- points along the specified vector.
--
jointMatrix :: Point3D -> Point3D -> Vector3D -> Matrix Float
jointMatrix base end vector = 
    let axis_vector = vectorNormalize $ vectorToFrom end base
        x_vector = vectorNormalize $ crossProduct (vectorNormalize vector) axis_vector
        y_vector = vectorNormalize $ crossProduct axis_vector x_vector
        in xyzMatrix x_vector y_vector axis_vector