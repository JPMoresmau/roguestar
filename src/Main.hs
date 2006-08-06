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

module Main
    (main)
    where

import Data.Maybe
import Translation
import Graphics.UI.GLUT
--import Graphics.Rendering.OpenGL.GL
--import Graphics.Rendering.OpenGL.GLU
--import Model
import StarflightBackground
import AscensionClassStarship

default_window_size :: Size
default_window_size = Size 640 480

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

languageFromArgs :: [String] -> Language
languageFromArgs [] = English
languageFromArgs (x:xs) = fromMaybe (languageFromArgs xs) (translator x)

main :: IO ()
main = do (_,args) <- getArgsAndInitialize
	  let trl = tr $ languageFromArgs args
	      in do initialWindowSize $= default_window_size
		    initialDisplayMode $= display_mode
		    window <- createWindow (trl ["window-title"])
		    reshapeCallback $= Just roguestarReshapeCallback
		    displayCallback $= roguestarDisplayCallback
		    addTimerCallback timer_callback_millis (roguestarTimerCallback window)
		    mainLoop

roguestarReshapeCallback :: Size -> IO ()
roguestarReshapeCallback (Size width height) = do matrixMode $= Projection
						  loadIdentity
						  viewport $= (Position 0 0,Size width height)
						  postRedisplay Nothing

roguestarDisplayCallback :: IO ()
roguestarDisplayCallback = do renderStarflightFlyby 0.0 ascension_class_starship
			      swapBuffers

{-
roguestarDisplayCallback :: IO ()
roguestarDisplayCallback = do clearColor $= Color4 0 0 0 0
			      clear [ColorBuffer,DepthBuffer]
			      depthFunc $= Just Lequal
			      depthMask $= Enabled
			      lighting $= Enabled
			      shadeModel $= Smooth
			      lightModelAmbient $= (Color4 0 0 0 0)
			      (light $ Light 0) $= Enabled
			      (ambient $ Light 0) $= (Color4 0 0 0 0)
			      (specular $ Light 0) $= (Color4 0 0 0 0)
			      (diffuse $ Light 0) $= (Color4 1 1 1 0)
			      (position $ Light 0) $= (Vertex4 3 3 3 0)
			      matrixMode $= Projection
			      loadIdentity
			      (Size width height) <- get windowSize
			      perspective 90.0 ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) 0.1 20.0
			      matrixMode $= Modelview 0
			      loadIdentity
			      lookAt (Vertex3 0 3 3) (Vertex3 0 0 0) (Vector3 0 1 0)
			      renderGoblet
			      swapBuffers
-}

roguestarTimerCallback :: Window -> IO ()
roguestarTimerCallback window = do addTimerCallback timer_callback_millis (roguestarTimerCallback window)
				   postRedisplay $ Just window

--renderGoblet :: IO ()
--renderGoblet = toOpenGL goblet
