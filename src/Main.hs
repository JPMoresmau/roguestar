{-# LANGUAGE Arrows #-}

module Main
    (main)
    where

import System.IO
import PrintText
import Data.Maybe
import Graphics.UI.GLUT
import Control.Monad
import Actions
import Keymaps.Keymaps
import CommandLine
import Keymaps.BuiltinKeymaps
import RenderingControl
import Driver
import Animation
import RSAGL.Scene
import Models.Library
import System.Timeout
import System.Exit
import Globals
import Control.Concurrent.STM
import Control.Concurrent
import Statistics

roguestar_client_version :: String
roguestar_client_version = "0.3"

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
                WithDepthBuffer,
                DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

main :: IO ()
main =
    do (_, command_line) <- getArgsAndInitialize
       let command_line_options = parseCommandLine command_line
       let keymap = findKeymapOrDefault $ keymap_name command_line_options
       scene_var <- newTVarIO Nothing
       globals <- defaultGlobals
       driver_object <- newDriverObject
       print_text_object <- newPrintTextObject
       animation_object <- newRoguestarAnimationObject mainAnimationLoop
       lib <- newLibrary
       initialWindowSize $= default_window_size
       initialDisplayMode $= display_mode
       window <- createWindow $ "RogueStar GL " ++ roguestar_client_version
       reshapeCallback $= Just roguestarReshapeCallback
       display_statistics <- newStatistics "rendering"
       displayCallback $= roguestarDisplayCallback display_statistics scene_var print_text_object
       perWindowKeyRepeat $= PerWindowKeyRepeatOff
       keyboardMouseCallback $= (Just $ keyCallback print_text_object)
       addTimerCallback timer_callback_millis (roguestarTimerCallback scene_var globals driver_object print_text_object keymap window)
       scene_statistics <- newStatistics "scene"
       sceneLoop scene_statistics scene_var lib globals driver_object print_text_object animation_object
       mainLoop

watchQuit :: Globals -> IO ()
watchQuit g =
    do q <- atomically $ readTVar $ global_should_quit g
       when q $ exitWith ExitSuccess

sceneLoop :: Statistics -> TVar (Maybe Scene) -> Library -> Globals -> DriverObject -> PrintTextObject -> RoguestarAnimationObject -> IO ()
sceneLoop stats scene_var lib globals driver_object print_text_object animation_object = liftM (const ()) $ forkIO $ forever $
    do atomically $
           do b <- liftM isJust $ readTVar scene_var
              when b retry
       result <- timeout 20000000 $
           do scene <- runStatistics stats $ runRoguestarAnimationObject lib globals driver_object print_text_object animation_object 
              atomically $ writeTVar scene_var $ Just scene
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled simulation run (timed out after 20 seconds)"
                   exitWith $ ExitFailure 1
           else return ()

roguestarReshapeCallback :: Size -> IO ()
roguestarReshapeCallback (Size width height) = 
    do matrixMode $= Projection
       loadIdentity
       viewport $= (Position 0 0,Size width height)

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
    do watchQuit globals
       result <- timeout 20000000 $
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

