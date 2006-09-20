module CameraTracking 
    (Camera(..),
     trackCamera,
     cameraLookAt)
    where

import Math3D
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU
import Data.List

data Camera = Camera { camera_spot, camera_position :: Point3D }

-- |
-- Calls GLU's lookAt with this camera.
--
cameraLookAt :: Camera -> IO ()
cameraLookAt (Camera { camera_spot=(Point3D spot_x spot_y spot_z), camera_position=(Point3D position_x position_y position_z)}) =
    lookAt (Vertex3 (cast position_x) (cast position_y) (cast position_z)) (Vertex3 (cast spot_x) (cast spot_y) (cast spot_z)) (Vector3 0 0 1)
	where cast = fromRational . toRational

-- |
-- Moves the camera the specified number of units to aim toward the specified point.
--
trackCamera :: Float -> [Point3D] -> Camera -> Camera
trackCamera delta targets camera0 = 
    let target = centerOfGravity targets
	new_spot = trackTarget delta target $ camera_spot camera0
	new_position = trackTarget delta (cameraPullback target targets) $ camera_position camera0
	in Camera { camera_spot=new_spot, camera_position=new_position }

trackTarget :: Float -> Point3D -> Point3D -> Point3D
trackTarget delta_step goal now = 
    let delta_vector = vectorToFrom goal now
	in if vectorLength delta_vector < delta_step
	   then goal
	   else Math3D.translate (vectorScaleTo delta_step $ delta_vector) now


centerOfGravity :: [Point3D] -> Point3D
centerOfGravity targets =
    let (xs,ys,zs) = unzip3 $ map toXYZ targets
	average nums = sum nums / (fromInteger $ genericLength nums)
	in (Point3D (average xs) (average ys) (average zs))

cameraPullback :: Point3D -> [Point3D] -> Point3D
cameraPullback (Point3D x y _) targets =
    let (xs,_,zs) = unzip3 $ map toXYZ targets
	breadth = maximum xs - minimum xs
	z = minimum zs
	in (Point3D x (y + breadth + 3) (z - 1.3*breadth - 4))