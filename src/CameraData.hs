module CameraData
   (Camera(..))
   where
   
import Math3D

data Camera = Camera { camera_spot, camera_position :: Point3D } deriving (Eq,Show)

instance AffineTransformable Camera where
    transform mat camera = Camera { camera_spot = transform mat $ camera_spot camera,
                                    camera_position = transform mat $ camera_position camera }

instance Lerpable Camera where
    lerp u (a,b) = Camera { camera_spot = lerp u (camera_spot a,camera_spot b),
                            camera_position = lerp u (camera_position a,camera_position b) }