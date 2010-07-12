module Processes
    (sceneLoop,
     reshape,
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

-- | Performs the listen-animate loop.  Should be called
-- exactly once per program instance.
sceneLoop :: Initialization -> IO ()
sceneLoop init = liftM (const ()) $ forkIO $ forever $
    do atomically $
           do b <- liftM isJust $ readTVar $ init_scene_var init
              when b retry
       result <- timeout 20000000 $
           do scene <- runStatistics (init_scene_statistics init) $
                  runRoguestarAnimationObject
                      (init_library init)
                      (init_globals init)
                      (init_driver_object init)
                      (init_print_text_object init)
                      (init_animation_object init)
              atomically $ writeTVar (init_scene_var init) $ Just scene
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled simulation run (timed out after 20 seconds)"
                   exitWith $ ExitFailure 1
           else return ()

-- | Update aspect ration, when it changes.
reshape :: Size -> IO ()
reshape (Size width height) =
    do matrixMode $= Projection
       loadIdentity
       viewport $= (Position 0 0,Size width height)

-- | Monitors the 'global_should_quit' variable,
-- and forcably terminates the application if
-- it is ever set.
watchQuit :: Initialization -> IO ()
watchQuit init_values = liftM (const ()) $ forkIO $ forever $
    do q <- atomically $
           do q <- readTVar $ global_should_quit $ init_globals init_values
              when (not q) retry
              return q
       when q $ exitWith ExitSuccess

-- | Performs the display action.  This must
-- be executed in the event loop of the widget toolkit,
-- since OpenGL isn't thread safe.
display :: Size -> Initialization -> IO ()
display (Size width height) init_vars =
  do scene <- atomically $
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
                   renderText $ init_print_text_object init_vars
     if isNothing result
          then do hPutStrLn stderr "roguestar-gl: aborting due to stalled display callback (timed out after 20 seconds)"
                  exitWith $ ExitFailure 1
          else return ()



