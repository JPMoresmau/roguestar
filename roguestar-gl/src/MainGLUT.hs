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
import System.Timeout
import System.Exit
import Globals
import Control.Concurrent.STM
import Statistics
import Config
import Initialization
import Processes
import RSAGL.Scene

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
       displayCallback $= theDisplayCallback init_vars
       perWindowKeyRepeat $= PerWindowKeyRepeatOff
       keyboardMouseCallback $=
           (Just $ keyCallback $ init_print_text_object init_vars)
       addTimerCallback timer_callback_millis $
           roguestarTimerCallback init_vars
                                  window
       sceneLoop init_vars
       mainLoop

theDisplayCallback :: Initialization -> IO ()
theDisplayCallback init_vars =
    do sz <- get windowSize
       display sz init_vars
       swapBuffers

roguestarTimerCallback :: Initialization -> Window -> IO ()
roguestarTimerCallback init_vars window =
    do addTimerCallback timer_callback_millis $
           roguestarTimerCallback init_vars window
       postRedisplay $ Just window
       dispatchKeyInput init_vars
       watchQuit init_vars

