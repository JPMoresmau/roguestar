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

module StarflightBackground
    (renderStarflightBackground,
     renderStarflightRotation)
    where

import Time
import Quality
import Graphics.UI.GLUT
import System.Random
import Data.List
import Model
import OGLStateConfiguration
import Data.IORef
import Globals

-- |
-- Renders an animated background as though traveling through hyperspace.
-- Note: Clears the color buffer and sets OpenGL state.
--				
starflight_background_ogl_configuration :: OGLStateConfiguration
starflight_background_ogl_configuration = ogl_state_configuration_effects { ogl_depth_func = Nothing,
									    ogl_near_plane = 1,
									    ogl_far_plane = 1000,
									    ogl_fog = Enabled,
									    ogl_fog_mode = Linear 500 1000 }

renderStarflightBackground :: (IORef RoguestarGlobals) -> IO ()
renderStarflightBackground globals_ref = 
    do setOpenGLState starflight_background_ogl_configuration
       clear [ColorBuffer]
       rotate_cycle <- cycleSeconds 360
       flow_cycle <- cycleSeconds 7
       lookAt (Vertex3 0 0 (flow_cycle*1000)) (Vertex3 0 0 2000) (Vector3 (sin $ 2*pi*rotate_cycle) (cos $ 2*pi*rotate_cycle) 0)
       preservingMatrix renderStarsFar
       preservingMatrix renderStarsNear
       preservingMatrix renderStarsBehind
	   where renderStarsNear = do globals <- readIORef globals_ref
				      renderStars (-1000,1000) (-1000,1000) (1,1000) $ starfield (global_quality globals)
		 renderStarsFar = do translate $ (Vector3 0 0 1000 :: Vector3 Double)
				     renderStarsNear
		 renderStarsBehind = do translate $ (Vector3 0 0 (-1000) :: Vector3 Double)
					renderStarsNear
				

starflight_model_ogl_configuration :: OGLStateConfiguration
starflight_model_ogl_configuration = 
    ogl_state_configuration_model {
				   ogl_fov_degrees = 45,
				   ogl_near_plane = 30,
				   ogl_far_plane = 300,
				   ogl_fog = Enabled,
				   ogl_fog_mode = Linear 100 200,
				   ogl_light_0 = Just OGLLightConfiguration {
									     ogl_light_ambient = Color4 0.1 0.1 0.2 0,
									     ogl_light_diffuse = Color4 1 1 1 0,
									     ogl_light_specular = Color4 1 1 1 0,
									     ogl_light_position = Vertex4 2000 2000 (-2000) 0
									    } }


-- |
-- Renders a model spinning through hyperspace.
--
renderStarflightRotation :: IO () -> (IORef RoguestarGlobals) -> IO ()
renderStarflightRotation display_fn globals_ref = 
    do renderStarflightBackground globals_ref
       setOpenGLState starflight_model_ogl_configuration
       clear [DepthBuffer]
       lookAt (Vertex3 0 0 0) (Vertex3 0 0 1) (Vector3 0 1 0)
       preservingMatrix $ renderModel
	   where renderModel = do cyc <- (cycleSeconds 35 :: IO GLfloat)
				  cyc_axis <- (cycleSeconds 5 :: IO GLfloat)
				  translate $ (Vector3 0 0 150 :: Vector3 GLfloat)
				  rotate (360*cyc) (Vector3 1 (sin (2*pi*cyc_axis)) 0)
				  display_fn

-- This isn't properly random, but it does create a nifty effect.
starfield :: Quality -> [(Double,Double,Double)]
starfield quality = let quality_n = case quality of
						 Bad -> 5
						 Poor -> 20
						 Good -> 100
						 Super -> 1000
			in sortBy (\(x0,y0,z0) (x1,y1,z1) -> compare (x1*x1+y1*y1+z1*z1) (x0*x0+y0*y0*z0*z0)) $ genStars (mkStdGen 2) quality_n

genStars :: StdGen -> Int -> [(Double,Double,Double)]
genStars _ 0 = []
genStars g0 n = let (x,g1) = next g0
		    (y,g2) = next g1
		    (z,g3) = next g2
		    tform = fromInteger . toInteger
		    in (tform (x `mod` 1000) / 1000.0,
			tform (y `mod` 1000) / 1000.0,
			tform (z `mod` 1000) / 1000.0) : (genStars g3 (n-1))

renderStars :: (Double,Double) -> (Double,Double) -> (Double,Double) -> [(Double,Double,Double)] -> IO ()
renderStars (minx,maxx) (miny,maxy) (minz,maxz) pts =
    do renderPrimitive Triangles $ mapM_ drawPoint pts
	   where drawPoint (x,y,z) = do color $ (Color4 1 1 1 1 :: Color4 Double)
				        (vertex $ Vertex3 
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz))
					color $ (Color4 0 0 1 0 :: Color4 Double)
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx-3)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz+500))
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx+3)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz+500))
					color $ (Color4 1 1 1 1 :: Color4 Double)
				        (vertex $ Vertex3 
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny)
					 (z*(maxz-minz)+minz))
					color $ (Color4 0 0 1 0 :: Color4 Double)
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny-6)
					 (z*(maxz-minz)+minz+250))
					(vertex $ Vertex3
					 (x*(maxx-minx)+minx)
					 (y*(maxy-miny)+miny+6)
					 (z*(maxz-minz)+minz+250))
