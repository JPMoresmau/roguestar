module RenderingControl
    (setNextDisplayFunc,
     initialTurnDisplay)
    where

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
       when (global_dones globals > dones_count) $ setNextDisplayFunc globals_ref initialTurnDisplay

-- |
-- Function that dispatches control to another display function based on the game engine's state.
--
initialTurnDisplay :: IORef RoguestarGlobals -> IO ()
initialTurnDisplay globals_ref = 
    do renderStarflightBackground globals_ref
       engine_state <- driverRequestAnswer globals_ref "state"
       case engine_state of
			 Just "race-selection" -> setNextDisplayFunc globals_ref initialRaceSelectionDisplay
			 Just "class-selection" -> setNextDisplayFunc globals_ref initialClassSelectionDisplay
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
			  printTable "player-stats" "0" $ 
			  waitNextTurnTransition renderStarflightBackground

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