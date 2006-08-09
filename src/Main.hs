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

import License
import PrintText
import Quality
import Data.IORef
import Globals
import Data.Maybe
import Translation
import Graphics.UI.GLUT
import StarflightBackground

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

languageFromArgs :: [String] -> Language
languageFromArgs [] = English
languageFromArgs (x:xs) = fromMaybe (languageFromArgs xs) (translator x)

qualityFromArgs :: [String] -> Quality
qualityFromArgs [] = Good
qualityFromArgs ("quality-bad":_) = Bad
qualityFromArgs ("quality-poor":_) = Poor
qualityFromArgs ("quality-good":_) = Good
qualityFromArgs ("quality-super":_) = Super
qualityFromArgs args = qualityFromArgs $ tail args

main :: IO ()
main = do (_,args) <- getArgsAndInitialize
	  let trl = tr $ languageFromArgs args
	      quality = qualityFromArgs args
	      in do globals_ref <- newIORef $ RoguestarGlobals { global_quality = quality,
								 global_display_func = renderStarflightBackground quality,
								 global_text_output_buffer = [],
								 global_text_output_mode = Unlimited }
		    printText globals_ref GUIMessage license_info
		    initialWindowSize $= default_window_size
		    initialDisplayMode $= display_mode
		    window <- createWindow (trl ["window-title"])
		    reshapeCallback $= Just roguestarReshapeCallback
		    displayCallback $= roguestarDisplayCallback globals_ref
		    addTimerCallback timer_callback_millis (roguestarTimerCallback globals_ref window)
		    mainLoop

roguestarReshapeCallback :: Size -> IO ()
roguestarReshapeCallback (Size width height) = do matrixMode $= Projection
						  loadIdentity
						  viewport $= (Position 0 0,Size width height)

roguestarDisplayCallback :: (IORef RoguestarGlobals) -> IO ()
roguestarDisplayCallback globals_ref = do globals <- readIORef globals_ref
					  (global_display_func globals)
					  renderText globals_ref
					  swapBuffers

roguestarTimerCallback :: (IORef RoguestarGlobals) -> Window -> IO ()
roguestarTimerCallback globals_ref window = do addTimerCallback timer_callback_millis (roguestarTimerCallback globals_ref window)
					       postRedisplay $ Just window