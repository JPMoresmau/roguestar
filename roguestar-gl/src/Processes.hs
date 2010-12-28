module Processes
    (sceneLoop,
     watchQuit,
     display)
    where

import Initialization
import Globals
import Control.Concurrent
import Control.Concurrent.STM
import System.Timeout
import Control.Monad
import Data.Maybe
import Statistics
import Animation
import System.IO
import System.Exit
import Graphics.Rendering.OpenGL.GL
import RSAGL.Scene
import PrintText
import Actions
import Keymaps.Keymaps

-- | Performs the listen-animate loop.  Should be called
-- exactly once per program instance.
sceneLoop :: Initialization -> IO ()
sceneLoop init_vars = liftM (const ()) $ forkIO $ forever $
    do dispatchKeyInput init_vars
       atomically $
           do b <- liftM isJust $ readTVar $ init_scene_var init_vars
              when b retry
       result <- timeout 20000000 $
           do scene <- runStatistics (init_scene_statistics init_vars) $
                  runRoguestarAnimationObject
                      (init_library init_vars)
                      (init_globals init_vars)
                      (init_driver_object init_vars)
                      (init_print_text_object init_vars)
                      (init_animation_object init_vars)
              atomically $ writeTVar (init_scene_var init_vars) $ Just scene
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled simulation run (timed out after 20 seconds)"
                   exitWith $ ExitFailure 1
           else return ()

-- | Update aspect ratio, when it changes.
reshape :: Size -> IO ()
reshape (Size width height) =
    do mat_mode <- get matrixMode
       matrixMode $= Projection
       loadIdentity
       viewport $= (Position 0 0,Size width height)
       matrixMode $= mat_mode

-- | Monitors the 'global_should_quit' variable,
-- and forcably terminates the application if
-- it is set.
--
-- Needs to be called from the main thread, or has
-- no effect.
watchQuit :: Initialization -> IO ()
watchQuit init_values =
    do q <- atomically $ readTVar $ global_should_quit $ init_globals init_values
       when q $ exitWith ExitSuccess

-- | Performs the display action.  This must
-- be executed in the event loop of the widget toolkit,
-- since OpenGL isn't thread safe.
display :: Size -> Initialization -> IO ()
display (Size width height) init_vars =
  do reshape (Size width height)
     scene <- atomically $
         do result <- maybe retry return =<< readTVar (init_scene_var init_vars)
            writeTVar (init_scene_var init_vars) Nothing
            return result
     result <- timeout 20000000 $
         do color (Color4 0 0 0 0 :: Color4 GLfloat)
            clear [ColorBuffer]
            runStatistics (init_display_statistics init_vars) $
                do sceneToOpenGL (fromIntegral width / fromIntegral height)
                                 (0.1,80.0)
                                 scene
                   renderText (Size width height) $
                              init_print_text_object init_vars
     if isNothing result
          then do hPutStrLn stderr "roguestar-gl: aborting due to stalled display callback (timed out after 20 seconds)"
                  exitWith $ ExitFailure 1
          else return ()

dispatchKeyInput :: Initialization -> IO ()
dispatchKeyInput init_vars =
    do result <- timeout 20000000 $
           do let action_input = ActionInput (init_globals init_vars)
                                             (init_driver_object init_vars)
                                             (init_print_text_object init_vars)
              pullInputBuffer (init_print_text_object init_vars)
              buffer_contents <- getInputBuffer (init_print_text_object init_vars)
              (id =<<) $ atomically $
                  do synced_with_engine <- liftM (not . null) $
                         getValidActions action_input Nothing
                     worked <- if synced_with_engine
                               then takeUserInputAction action_input =<<
                                        keysToActionNames action_input
                                                          (init_keymap init_vars)
                                                          buffer_contents
                               else return False
                     filtered <- filterKeySequence action_input
                                                   (init_keymap init_vars)
                                                   buffer_contents
                     return $ if worked
                              then clearInputBuffer (init_print_text_object init_vars)
                              else setInputBuffer (init_print_text_object init_vars)
                                                  filtered
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled timer callback (timed out after 20 seconds)"
                   exitWith $ ExitFailure 1
           else return ()

