module Joint
    (joint,
     basicArm) where
    
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
                     joint_hand :: Matrix Float }
                       deriving (Show)

-- |
-- Calculate where the joint would be in an articulated arm, given the arm's length,
-- a bend vector, and assuming that the joint is 1 unit in length.
-- The bend vector is the direction in which the join goes away from the line from
-- the base of the arm to the end of the arm.  For example, a human leg's bend
-- vector would point up and forward, while a human arm's bend vector would point
-- down and backwards.  Of course, in real life, it's easy to twist one's arm to give 
-- it a new bend vector.
--
joint :: Point3D -> Point3D -> Float -> Vector3D -> Joint
joint base end len bend_vector = 
    let joint_offset = sqrt $ len^2 - (distanceBetween base end)^2
        axis_vector = vectorNormalize $ vectorToFrom end base
        x_vector = crossProduct axis_vector (vectorNormalize bend_vector)
        y_vector = crossProduct axis_vector x_vector
        parametric_circle theta = vectorScale joint_offset $ vectorAdd (vectorScale (cos theta) x_vector) (vectorScale (sin theta) y_vector)
        joint_bend_result = translate (parametric_circle $ minimize (distanceBetween bend_vector . parametric_circle) (0,2*pi) 0.01) (lerp (0.5 :: Float) (base,end))
        hand_axis_vector = vectorToFrom joint_bend_result end
        hand_x_vector = crossProduct hand_axis_vector (vectorNormalize bend_vector)
        hand_y_vector = crossProduct hand_axis_vector hand_x_vector
        in Joint { joint_base = base,
                   joint_end = end,
                   joint_bend = joint_bend_result,
                   joint_hand = xyzMatrix hand_x_vector hand_y_vector hand_axis_vector } 
                   
-- |
-- A model of a basic arm based on a joint and thickness.
--
basicArm :: Joint -> Float -> Quality -> Texture -> Model
basicArm the_joint thickness q tex =
     qualityTube q tex [(joint_base the_joint,thickness),
                        (joint_bend the_joint,thickness),
                        (joint_end the_joint,thickness)]