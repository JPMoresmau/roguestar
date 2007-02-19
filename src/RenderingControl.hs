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

module RenderingControl
    (setNextDisplayFunc,
     displayDispatch)
    where

import OGLStateConfiguration
import Globals
import Data.IORef
import Control.Monad
import Data.Maybe
import Driver
import StarflightBackground
import PrintText
import Menus
import Tables
import PrintTables
import ObjectRenderer
import TerrainRenderer
import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU
import Math3D
import Camera
import Models.LibraryData
import Models.Library
import Quality
import Animation

-- |
-- Sets the next function to be used as the OpenGL display callback.  This function will
-- retain control until the next call to setNextDisplayFunc.
--
setNextDisplayFunc :: IORef RoguestarGlobals -> (IORef RoguestarGlobals -> IO ()) -> IO ()
setNextDisplayFunc globals_ref fn = modifyIORef globals_ref (\globals -> globals { global_display_func=fn })

-- |
-- Sets a display function to continue until the next done.  ("done" is the signal sent
-- from roguestar-engine when a state transition occurs.)
--
setOngoingDisplayFunc :: IORef RoguestarGlobals -> (IORef RoguestarGlobals -> IO()) -> IO ()
setOngoingDisplayFunc globals_ref fn =
    do dones <- liftM global_dones $ readIORef globals_ref
       let ongoingDisplayFunc _ = do dones_later <- liftM global_dones $ readIORef globals_ref
				     fn globals_ref
				     when (dones /= dones_later) $
					  setNextDisplayFunc globals_ref displayDispatch
       setNextDisplayFunc globals_ref ongoingDisplayFunc

-- |
-- Sets itself as the display function and then waits for the next "done" from the engine,
-- at which point it passes control to initialTurnDisplay.
--
waitNextTurnTransition :: (IORef RoguestarGlobals -> IO ()) -> IORef RoguestarGlobals -> IO ()
waitNextTurnTransition waiting_display_func globals_ref = 
    do waiting_display_func globals_ref
       globals <- readIORef globals_ref
       setNextDisplayFunc globals_ref $ waitNextTurnTransition_ (global_dones globals) waiting_display_func

waitNextTurnTransition_ :: Integer -> (IORef RoguestarGlobals -> IO ()) -> IORef RoguestarGlobals -> IO ()
waitNextTurnTransition_ dones_count waiting_display_func globals_ref =
    do waiting_display_func globals_ref
       driverRead globals_ref
       globals <- readIORef globals_ref
       when (global_dones globals > dones_count) $ setNextDisplayFunc globals_ref displayDispatch

-- |
-- Function that dispatches control to another display function based on the game engine's state.
--
displayDispatch :: IORef RoguestarGlobals -> IO ()
displayDispatch globals_ref = 
    do engine_state <- driverRequestAnswer globals_ref "state"
       case engine_state of
			 Just "race-selection" -> setNextDisplayFunc globals_ref initialRaceSelectionDisplay
			 Just "class-selection" -> setNextDisplayFunc globals_ref initialClassSelectionDisplay
			 Just "player-turn" -> setNextDisplayFunc globals_ref initialTurnDisplay
			 Just str -> do printText globals_ref Untranslated ("encountered unknown engine state:" ++ str)
					setNextDisplayFunc globals_ref renderStarflightBackground
			 Nothing -> return ()

-- |
-- Use this function to check that the engine is actual in the state(s) that we expect it to be.
-- If this function detects that it is not in such a state, then it will flip back to calling displayDispatch.
-- Otherwise we might get stuck.
--
stateGuard :: IORef RoguestarGlobals -> [String] -> IO ()
stateGuard globals_ref anticipated_states =
    do state <- driverGetAnswer globals_ref New "state"
       unless (maybe True (`elem` anticipated_states) state) $ 
               do printText globals_ref Untranslated "stateGuard: recovering..."
                  displayDispatch globals_ref

initialRaceSelectionDisplay :: IORef RoguestarGlobals -> IO ()
initialRaceSelectionDisplay globals_ref = 
    do stateGuard globals_ref ["race-selection"]
       renderStarflightBackground globals_ref
       table <- driverRequestTable globals_ref "player-races" "0"
       when (isJust table) $ do printMenu globals_ref "select-race" (tableSelect1 (fromJust table) "name")
				waitNextTurnTransition renderStarflightBackground globals_ref

initialClassSelectionDisplay :: IORef RoguestarGlobals -> IO ()
initialClassSelectionDisplay globals_ref =
    do stateGuard globals_ref ["class-selection"]
       renderStarflightBackground globals_ref
       setNextDisplayFunc globals_ref $ 
			  printTable "player-stats" "0" $ classSelectionMenuDisplay

classSelectionMenuDisplay :: IORef RoguestarGlobals -> IO ()
classSelectionMenuDisplay globals_ref =
    do stateGuard globals_ref ["class-selection"]
       table <- driverRequestTable globals_ref "base-classes" "0"
       when (isJust table) $ do printMenu globals_ref "select-base-class" $ (tableSelect1 (fromJust table) "class" ++ ["reroll"])
				waitNextTurnTransition renderStarflightBackground globals_ref

initialTurnDisplay :: IORef RoguestarGlobals -> IO ()
initialTurnDisplay globals_ref =
    do stateGuard globals_ref ["player-turn"]
       good <- resetTerrainRenderingFunction globals_ref
       ongoingTurnDisplay globals_ref
       when good $ setOngoingDisplayFunc globals_ref ongoingTurnDisplay

turn_display_configuration :: OGLStateConfiguration
turn_display_configuration = ogl_state_configuration_model { 
							    ogl_background_color = Color4 0.5 0.5 1.0 1.0,
							    ogl_near_plane = 2,
							    ogl_far_plane = 35,
							    ogl_fov_degrees = 40,
							    ogl_light_0 = 
							    Just $ OGLLightConfiguration { ogl_light_ambient = Color4 0.2 0.2 0.2 1.0,
											   ogl_light_diffuse = Color4 0.85 0.85 0.85 1.0,
											   ogl_light_position = Vertex4 0.5 0.5 (-1) 0,
											   ogl_light_specular = Color4 0.5 0.5 0.5 1.0 },
							    ogl_fog = Enabled,
							    ogl_fog_mode = Exp2 0.09
							   }

ongoingTurnDisplay :: IORef RoguestarGlobals -> IO ()
ongoingTurnDisplay globals_ref =
    do stateGuard globals_ref ["player-turn"]
       loadIdentity
       cameraLookAt . fromCSN world_coordinates =<< getCamera globals_ref
       setOpenGLState turn_display_configuration
       clear [ColorBuffer,DepthBuffer]
       globals <- readIORef globals_ref
       global_terrain_rendering_function globals globals_ref True
       clear [DepthBuffer]
       colorMask $= Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled
       global_terrain_rendering_function globals globals_ref False
       renderPrimitive TriangleFan $ do vertex $ Vertex3 (-10000) 0 (-10000 :: Double)
                                        vertex $ Vertex3 (-10000) 0 (10000 :: Double)
                                        vertex $ Vertex3 10000 0 (10000 :: Double)
                                        vertex $ Vertex3 10000 0 (-10000 :: Double)
       colorMask $= Color4 Enabled Enabled Enabled Enabled
       renderObjects globals_ref

-- |
-- Reads in (using Driver) and prints (using PrintText).
-- printTableInformation may not successfully read the table from the Driver.
-- If it does not, it does a setNextDisplayFunc on itself to try again next time.
-- If it does successfully read the table, it formats and prints
-- the information and then does a setNextDisplayFunc on the last parameter.
--
printTable :: String -> String -> (IORef RoguestarGlobals -> IO ()) -> IORef RoguestarGlobals -> IO ()
printTable the_table_name the_table_id next_display_func globals_ref =
    do renderStarflightBackground globals_ref
       globals <- readIORef globals_ref
       table <- driverRequestTable globals_ref the_table_name the_table_id
       let formatted_table = formatTable (global_language globals) $ fromJust table
       when (isJust table) $ do (if isJust formatted_table
				 then printText globals_ref Information $ fromJust formatted_table
				 else printText globals_ref Untranslated ("No format for table:" ++ (table_name $ fromJust table)))
				setNextDisplayFunc globals_ref next_display_func
       when (isNothing table) $ setNextDisplayFunc globals_ref $ printTable the_table_name the_table_id next_display_func

