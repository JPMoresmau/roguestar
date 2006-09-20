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
import TerrainRenderer
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU
import Math3D
import Seconds
import CameraTracking

-- |
-- Sets the next function to be used as the OpenGL display callback.  This function will
-- retain control until the next call to setNextDisplayFunc.
--
setNextDisplayFunc :: IORef RoguestarGlobals -> (IORef RoguestarGlobals -> IO ()) -> IO ()
setNextDisplayFunc globals_ref fn = do globals <- readIORef globals_ref
				       writeIORef globals_ref $ globals { global_display_func=fn }

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
    do renderStarflightBackground globals_ref
       engine_state <- driverRequestAnswer globals_ref "state"
       case engine_state of
			 Just "race-selection" -> setNextDisplayFunc globals_ref initialRaceSelectionDisplay
			 Just "class-selection" -> setNextDisplayFunc globals_ref initialClassSelectionDisplay
			 Just "player-turn" -> setNextDisplayFunc globals_ref initialTurnDisplay
			 Just str -> do printText globals_ref Untranslated ("encountered unknown engine state:" ++ str)
					setNextDisplayFunc globals_ref renderStarflightBackground
			 Nothing -> return ()

initialRaceSelectionDisplay :: IORef RoguestarGlobals -> IO ()
initialRaceSelectionDisplay globals_ref = 
    do renderStarflightBackground globals_ref
       table <- driverRequestTable globals_ref "player-races" "0"
       when (isJust table) $ do printMenu globals_ref "select-race" (tableSelect1 (fromJust table) "name")
				waitNextTurnTransition renderStarflightBackground globals_ref

initialClassSelectionDisplay :: IORef RoguestarGlobals -> IO ()
initialClassSelectionDisplay globals_ref =
    do renderStarflightBackground globals_ref
       setNextDisplayFunc globals_ref $ 
			  printTable "player-stats" "0" $ classSelectionMenuDisplay

classSelectionMenuDisplay :: IORef RoguestarGlobals -> IO ()
classSelectionMenuDisplay globals_ref =
    do table <- driverRequestTable globals_ref "base-classes" "0"
       when (isJust table) $ do printMenu globals_ref "select-base-class" $ (tableSelect1 (fromJust table) "class" ++ ["reroll"])
				waitNextTurnTransition renderStarflightBackground globals_ref

initialTurnDisplay :: IORef RoguestarGlobals -> IO ()
initialTurnDisplay globals_ref =
    do good <- resetTerrainRenderingFunction globals_ref
       updateCamera globals_ref [Point3D 0 0 0]
       turnDisplay globals_ref
       when good $ setNextDisplayFunc globals_ref turnDisplay

turn_display_configuration :: OGLStateConfiguration
turn_display_configuration = ogl_state_configuration_model { 
							    ogl_background_color = Color4 0.5 0.5 1.0 1.0,
							    ogl_near_plane = 2,
							    ogl_far_plane = 35,
							    ogl_light_0 = 
							    Just $ OGLLightConfiguration { ogl_light_ambient = Color4 0.2 0.2 0.2 1.0,
											   ogl_light_diffuse = Color4 1.0 1.0 1.0 1.0,
											   ogl_light_position = Vertex4 10000 10000 10000 1,
											   ogl_light_specular = Color4 1.0 1.0 1.0 1.0 },
							    ogl_fog = Enabled,
							    ogl_fog_mode = Exp2 0.08
							   }

turnDisplay :: IORef RoguestarGlobals -> IO ()
turnDisplay globals_ref =
    do setOpenGLState turn_display_configuration
       clear [ColorBuffer,DepthBuffer]
       updateCamera globals_ref [Point3D (-10) 20 (-50)]
       globals <- readIORef globals_ref
       global_terrain_rendering_function globals globals_ref

-- |
-- Moves the camera to look at the specified coordinates by calling lookAt.
--
updateCamera :: IORef RoguestarGlobals -> [Point3D] -> IO ()
updateCamera globals_ref spot_targets =
    do now_seconds <- seconds
       delta_seconds <- liftM ((now_seconds -) . global_last_camera_update_seconds) $ readIORef globals_ref
       new_camera <- liftM (trackCamera (fromRational delta_seconds) spot_targets . global_camera) $ readIORef globals_ref
       modifyIORef globals_ref ( \ globals -> globals { global_last_camera_update_seconds = now_seconds,
							global_camera = new_camera } )
       cameraLookAt new_camera

-- |
-- Reads in (using Driver) and prints (using PrintText).
-- printTableInformation may not successfully read the table from the Driver.  It it does not,
-- it returns taking no action.  If it does successfully read the table, it formats and prints
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

