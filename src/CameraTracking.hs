--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module CameraTracking 
    (Camera(..),
     trackCamera,
     cameraLookAt,
     lookAtCamera)
    where

import Math3D
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Data.List

data Camera = Camera { camera_spot, camera_position :: Point3D } deriving (Show)

-- |
-- Take a function that renders a shape in the x-y plane (it's ok for the shape to extend along the z-axis,
-- but the intention is that such an extention will be barely perceptable).  Always rotate the shape so that
-- it faces the camera.  This function does this.
--
-- (lookAtCamera camera xyz fn) calls the function fn in OpenGL so that it will be translated to xyz and rotated
-- to face the camera.
--
lookAtCamera :: (Xyz a) => Camera -> a -> IO () -> IO ()
lookAtCamera camera xyz_source rendering_fn =
  do let xyz@(x,y,z) = toXYZ xyz_source
         vector_to_camera@(Vector3D delta_x delta_y _) = vectorToFrom (camera_position camera) (point3d xyz)
         rotateY = (* signum (-delta_x)) $ (* 180) $ (/ 3.14) $ angleBetween (Vector3D 0 0 (-1)) (Math3D.scale (Vector3D 1 0 1) vector_to_camera) -- angle around
         rotateX = (* signum delta_y) $ (* 180) $ (/ 3.14) $ angleBetween (Vector3D 0 0 (-1)) (Math3D.scale (Vector3D 0 1 1) vector_to_camera) -- angle up & down
     preservingMatrix $
         do GL.translate $ Vector3 x y z
            GL.rotate rotateX $ Vector3 1 0 0
            GL.rotate rotateY $ Vector3 0 1 0
            rendering_fn
            

-- |
-- Calls GLU's lookAt with this camera.
--
cameraLookAt :: Camera -> IO ()
cameraLookAt (Camera { camera_spot=(Point3D spot_x spot_y spot_z), camera_position=(Point3D position_x position_y position_z)}) =
    lookAt (Vertex3 (cast position_x) (cast position_y) (cast position_z)) (Vertex3 (cast spot_x) (cast spot_y) (cast spot_z)) (Vector3 0 (1.61^2) (1))
	where cast = fromRational . toRational

-- |
-- Moves the camera the specified number of units to aim toward the specified point.
--
trackCamera :: Float -> [Point3D] -> Camera -> Camera
trackCamera delta targets camera0 = 
    let target = centerOfGravity targets
	new_spot = trackTarget (delta*1.61) target $ camera_spot camera0 -- spot moves faster than camera
	new_position = trackTarget delta (cameraPullback target targets) $ camera_position camera0
	in Camera { camera_spot=new_spot, camera_position=new_position }

-- |
-- trackTarget delta goal now
-- Where delta is the maximum distance to move on a single step, goal is the goal
-- position, and now is the current position, returns the next position approaching
-- the goal on a linear path.
--
trackTarget :: Float -> Point3D -> Point3D -> Point3D
trackTarget delta_step goal now = 
    let delta_vector = vectorToFrom goal now
	in if vectorLength delta_vector < delta_step
	   then goal
	   else Math3D.translate (vectorScaleTo delta_step $ delta_vector) now


-- |
-- The center of gravity of a group of points, assuming all points have equal weight.
-- That is, the average position of all the points.
--
centerOfGravity :: [Point3D] -> Point3D
centerOfGravity targets =
    let (xs,ys,zs) = unzip3 $ map toXYZ targets
	average nums = sum nums / (fromInteger $ genericLength nums)
	in (Point3D (average xs) (average ys) (average zs))

-- |
-- Where the camera should be, in relation a primary target and all other targets.
--
cameraPullback :: Point3D -> [Point3D] -> Point3D
cameraPullback (Point3D x y _) targets =
    let (xs,_,zs) = unzip3 $ map toXYZ targets
	breadth = maximum xs - minimum xs
	z = minimum zs
	in (Point3D x (y + breadth + 3) (z - 1.3*breadth - 4))