{-# LANGUAGE Arrows, OverloadedStrings #-}

module Main
    (main)
    where

import System.IO
import PrintText
import Data.Maybe
import Graphics.UI.GLUT hiding (initialize)
import Control.Monad
import Actions
import Keymaps.Keymaps
import Driver
import RSAGL.Scene
import System.Timeout
import System.Exit
import Globals
import Control.Concurrent.STM
import Statistics
import Config
import Initialization
import Processes

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
                WithDepthBuffer,
                DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

main :: IO ()
main =
    do (_, args) <- getArgsAndInitialize
       init_vars <- initialize args
       let (width,height) = default_window_size
       initialWindowSize $= Size width height
       initialDisplayMode $= display_mode
       window <- createWindow window_name
       reshapeCallback $= Just reshape
       displayCallback $= roguestarDisplayCallback
           (init_display_statistics init_vars)
           (init_scene_var init_vars)
           (init_print_text_object init_vars)
       perWindowKeyRepeat $= PerWindowKeyRepeatOff
       keyboardMouseCallback $=
           (Just $ keyCallback $ init_print_text_object init_vars)
       addTimerCallback timer_callback_millis $
           roguestarTimerCallback (init_scene_var init_vars)
                                  (init_globals init_vars)
                                  (init_driver_object init_vars)
                                  (init_print_text_object init_vars)
                                  (init_keymap init_vars)
                                  window
       sceneLoop init_vars
       watchQuit init_vars
       mainLoop

roguestarDisplayCallback :: Statistics -> TVar (Maybe Scene) -> PrintTextObject -> IO ()
roguestarDisplayCallback stats scene_var print_text_object =
  do scene <- atomically $
         do result <- maybe retry return =<< readTVar scene_var
            writeTVar scene_var Nothing
            return result
     result <- timeout 20000000 $
         do color (Color4 0 0 0 0 :: Color4 GLfloat)
            clear [ColorBuffer]
            (Size width height) <- get windowSize
            runStatistics stats $
                do sceneToOpenGL (fromIntegral width / fromIntegral height) (0.1,80.0) scene
                   renderText print_text_object
                   swapBuffers
     if isNothing result
         then do hPutStrLn stderr "roguestar-gl: aborting due to stalled display callback (timed out after 20 seconds)"
                 exitWith $ ExitFailure 1
         else return ()

roguestarTimerCallback :: TVar (Maybe Scene) -> Globals -> DriverObject -> PrintTextObject -> Keymap -> Window -> IO ()
roguestarTimerCallback scene_var globals driver_object print_text_object keymap window =
    do result <- timeout 20000000 $
        do addTimerCallback timer_callback_millis $ roguestarTimerCallback scene_var globals driver_object print_text_object keymap window
           postRedisplay $ Just window
           maybeExecuteKeymappedAction globals driver_object print_text_object keymap
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled timer callback (timed out after 20 seconds)"
                   exitWith $ ExitFailure 1
           else return ()

maybeExecuteKeymappedAction :: Globals ->
                               DriverObject ->
                               PrintTextObject ->
                               Keymap ->
                               IO ()
maybeExecuteKeymappedAction globals driver_object print_text_object keymap =
    do let action_input = ActionInput globals
                                      driver_object
                                      print_text_object
       pullInputBuffer print_text_object
       buffer_contents <- getInputBuffer print_text_object
       (id =<<) $ atomically $
           do synced_with_engine <- liftM (not . null) $
                  getValidActions action_input Nothing
              worked <- if synced_with_engine
                        then takeUserInputAction action_input =<<
                                 keysToActionNames action_input keymap buffer_contents
                        else return False
              filtered <- filterKeySequence action_input
                                            keymap
                                            buffer_contents
              return $ if worked
                       then clearInputBuffer print_text_object
                       else setInputBuffer print_text_object filtered

