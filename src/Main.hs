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
    (main,
     displayModel)
    where

import System.IO
import License
import PrintText
import Quality
import Data.IORef
import Globals
import Data.Maybe
import Data.List
import Translation
import Graphics.UI.GLUT
import Control.Monad
import Actions
import Keymaps
import RenderingControl
import Model
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
languageFromArgs (x:xs) = fromMaybe (languageFromArgs xs) (stringToLanguage x)

qualityFromArgs :: [String] -> Quality
qualityFromArgs [] = Good
qualityFromArgs ("quality-bad":_) = Bad
qualityFromArgs ("quality-poor":_) = Poor
qualityFromArgs ("quality-good":_) = Good
qualityFromArgs ("quality-super":_) = Super
qualityFromArgs args = qualityFromArgs $ tail args

main :: IO ()
main = do (_,args) <- getArgsAndInitialize
	  language <- return $ languageFromArgs args
	  globals_ref <- newIORef $ roguestar_globals_0 { global_quality = qualityFromArgs args,
							  global_display_func = displayDispatch,
							  global_language = language }
	  printText globals_ref GUIMessage license_info
	  initialWindowSize $= default_window_size
	  initialDisplayMode $= display_mode
	  window <- createWindow (translateStr language ["window-title"])
	  reshapeCallback $= Just roguestarReshapeCallback
	  displayCallback $= roguestarDisplayCallback globals_ref
	  keyboardMouseCallback $= (Just $ roguestarKeyCallback globals_ref)
	  addTimerCallback timer_callback_millis (roguestarTimerCallback globals_ref window)
	  mainLoop

displayModel :: Model -> IO ()
displayModel model = 
    do globals_ref <- newIORef $ roguestar_globals_0 { global_display_func = renderStarflightRotation model }
       initialWindowSize $= default_window_size
       initialDisplayMode $= display_mode
       window <- createWindow "Roguestar-GL: Model Display"
       reshapeCallback $= Just roguestarReshapeCallback
       displayCallback $= roguestarDisplayCallback globals_ref
       addTimerCallback timer_callback_millis (roguestarTimerCallback globals_ref window)
       mainLoop

roguestarReshapeCallback :: Size -> IO ()
roguestarReshapeCallback (Size width height) = do matrixMode $= Projection
						  loadIdentity
						  viewport $= (Position 0 0,Size width height)

roguestarDisplayCallback :: IORef RoguestarGlobals -> IO ()
roguestarDisplayCallback globals_ref = do globals <- readIORef globals_ref
					  (global_display_func globals) globals_ref
					  renderText globals_ref
					  swapBuffers

roguestarTimerCallback :: IORef RoguestarGlobals -> Window -> IO ()
roguestarTimerCallback globals_ref window = do addTimerCallback timer_callback_millis (roguestarTimerCallback globals_ref window)
					       postRedisplay $ Just window

appendUserInputStr :: IORef RoguestarGlobals -> String -> IO ()
appendUserInputStr globals_ref str0 =
    do globals0 <- readIORef globals_ref
       str1 <- return $ (global_user_input globals0 ++ str0)
       str' <- filterKeySequence globals_ref str1
       globals1 <- readIORef globals_ref
       writeIORef globals_ref $ globals1 { global_user_input=str' }

roguestarKeyCallback :: IORef RoguestarGlobals -> KeyboardMouseCallback
roguestarKeyCallback _ _ Up _ _ = return ()
roguestarKeyCallback _ (MouseButton {}) _ _ _ = return ()

roguestarKeyCallback globals_ref (Char char) _ _ _ = 
    do appendUserInputStr globals_ref [char]
       maybeExecuteKeymappedAction globals_ref

roguestarKeyCallback globals_ref (SpecialKey special) _ _ _ =
    do appendUserInputStr globals_ref $ (show special)
       maybeExecuteKeymappedAction globals_ref

clearUserInput :: IORef RoguestarGlobals -> IO ()
clearUserInput globals_ref =
    do globals <- readIORef globals_ref
       writeIORef globals_ref $ globals { global_user_input="" }

maybeExecuteKeymappedAction :: IORef RoguestarGlobals -> IO ()
maybeExecuteKeymappedAction globals_ref =
    do globals <- readIORef globals_ref
       actions <- keysToActionNames globals_ref $ global_user_input globals
       worked <- takeUserInputAction globals_ref actions
       when worked $ clearUserInput globals_ref