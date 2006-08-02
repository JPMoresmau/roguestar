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
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU

default_window_size :: Size
default_window_size = Size 640 480

display_mode :: [DisplayMode]
display_mode = [RGBMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 100

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
roguestarDisplayCallback = do clearColor $= Color4 0 0 0 0
			      clear [ColorBuffer,DepthBuffer]
			      matrixMode $= Projection
			      loadIdentity
			      (Size width height) <- get windowSize
			      perspective 45.0 ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) 0.1 3.0
			      matrixMode $= Modelview 0
			      loadIdentity
			      lookAt (Vertex3 1 1 1) (Vertex3 0 0 0) (Vector3 0 1 0)
			      renderDumbSquare
			      swapBuffers

roguestarTimerCallback :: Window -> IO ()
roguestarTimerCallback window = do addTimerCallback timer_callback_millis (roguestarTimerCallback window)
				   postRedisplay $ Just window

renderDumbSquare :: IO ()
renderDumbSquare = do renderPrimitive Polygon renderDumbSquare_

renderDumbSquare_ :: IO ()
renderDumbSquare_ = do color $ (Color3 1.0 1.0 1.0 :: Color3 GLclampf)
		       vertex $ (Vertex3 (-0.5) 0 (-0.5) :: Vertex3 GLdouble)
		       vertex $ (Vertex3 (-0.5) 0 0.5 :: Vertex3 GLdouble)
		       vertex $ (Vertex3 0.5 0 0.5 :: Vertex3 GLdouble)
		       vertex $ (Vertex3 0.5 0 (-0.5) :: Vertex3 GLdouble)
