{-# OPTIONS_GHC -fglasgow-exts #-}

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

module Camera 
    (newCameraAnimation,
     cameraLookAt,
     lookAtCamera,
     module CameraData)
    where

import Math3D
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Data.List
import AnimationCore
import AnimationAux
import Control.Monad
import Tables
import Data.Maybe
import Time
import CameraData
import Data.IORef
import Globals

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
         rotate_y = scaleAngle (signum (-delta_x)) $ angleBetween (Vector3D 0 0 (-1)) (Math3D.scale (Vector3D 1 0 1) vector_to_camera) -- angle around
         rotate_x = scaleAngle (signum delta_y) $ angleBetween (Vector3D 0 0 (-1)) (Math3D.scale (Vector3D 0 1 1) vector_to_camera) -- angle up & down
     preservingMatrix $
         do GL.translate $ Vector3 x y z
            GL.rotate (inDegrees rotate_x) $ Vector3 1 0 0
            GL.rotate (inDegrees rotate_y) $ Vector3 0 1 0
            rendering_fn
            

-- |
-- Calls GLU's lookAt with this camera.
--
cameraLookAt :: Camera -> IO ()
cameraLookAt (Camera { camera_spot=(Point3D spot_x spot_y spot_z), camera_position=(Point3D position_x position_y position_z)}) =
    lookAt (Vertex3 (cast position_x) (cast position_y) (cast position_z)) (Vertex3 (cast spot_x) (cast spot_y) (cast spot_z)) (Vector3 0 (1.61^2) (1))
	where cast = fromRational . toRational

-- |
-- The Camera animation.
--
newCameraAnimation :: IORef RoguestarGlobals -> IO (Animation () (CSN Camera))
newCameraAnimation globals_ref = 
    newAcceleratedLerpAnimation 
        (0,0) 
        (toCSN world_coordinates Camera {camera_spot=(Point3D 0 0 1), camera_position=origin_point_3d})
        (centerCoordinates globals_ref)
        (\_ (x,y) -> return $ toCSN world_coordinates $ goalCamera [Point3D (fromIntegral x) 0 (fromIntegral y)])
        (\_ -> return ())
        (\x y -> return $ fromSeconds $ limitTime $ distanceBetween (camera_position $ fromCSN world_coordinates x) (camera_position $ fromCSN world_coordinates y))
            where limitTime x | x > 4 = 4 + log (x - 4)
                  limitTime x = x

camera_states :: [String]
camera_states = ["player-turn"]  -- states that provide center-coordinates 
                                 -- if we get a bug where the camera stops moving, we probably need to add a state here

centerCoordinates :: IORef RoguestarGlobals -> AniM i o (Maybe (Integer,Integer))
centerCoordinates globals_ref = 
    do maybe_state <- animGetAnswer globals_ref Fresh "state"
       if (maybe False (`elem` camera_states) maybe_state)  
           then do maybe_table <- animGetTable globals_ref Anything "center-coordinates" "0"
                   return $ do table <- maybe_table
		               coords <- return $ tableSelect2Integer table ("axis","coordinate")
		               maybe_x <- lookup "x" coords
		               maybe_y <- lookup "y" coords
		               liftM2 (,) maybe_x maybe_y
           else return Nothing

-- |
-- The goal camera based on a group of points that we want in the field of view.
--
goalCamera :: [Point3D] -> Camera
goalCamera targets = Camera { camera_spot=goal_spot, camera_position=goal_position }
                         where goal_spot = centerOfGravity targets
                               goal_position = cameraPullback goal_spot targets
 
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
-- Where the camera should be, in relation to a primary target and all other targets.
--
cameraPullback :: Point3D -> [Point3D] -> Point3D
cameraPullback (Point3D x y _) targets =
    let (xs,_,zs) = unzip3 $ map toXYZ targets
	breadth = maximum xs - minimum xs
	z = minimum zs
	in (Point3D x (y + 0.8*breadth + 3) (z - 1.3*breadth - 4))