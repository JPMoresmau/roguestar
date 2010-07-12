module Processes
    (sceneLoop,
     reshape)
    where

import Initialization
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




