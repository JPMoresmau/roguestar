{-# LANGUAGE Arrows, OverloadedStrings #-}

module MainGLUT
    (mainGLUT)
    where

import System.IO
import PrintText
import Data.Maybe
import Graphics.UI.GLUT as GLUT hiding (initialize)
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
import KeyStroke as KeyStroke
import DrawString

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
                WithDepthBuffer,
                DoubleBuffered]

mainGLUT :: IO ()
mainGLUT =
    do (_, args) <- getArgsAndInitialize
       init_vars <- initialize glutDrawString args
       let (width,height) = default_window_size
       initialWindowSize $= Size width height
       initialDisplayMode $= display_mode
       window <- createWindow window_name
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
       watchQuit init_vars

keyCallback :: PrintTextObject -> KeyboardMouseCallback
keyCallback pto (Char char) Down _ _ =
    pushInputBuffer pto (Stroke char)
keyCallback pto (SpecialKey GLUT.KeyDown) Down _ _ =
    pushInputBuffer pto KeyStroke.KeyDown
keyCallback pto (SpecialKey GLUT.KeyUp) Down _ _ =
    pushInputBuffer pto KeyStroke.KeyUp
keyCallback pto (SpecialKey GLUT.KeyLeft) Down _ _ =
    pushInputBuffer pto KeyStroke.KeyLeft
keyCallback pto (SpecialKey GLUT.KeyRight) Down _ _ =
    pushInputBuffer pto KeyStroke.KeyRight
keyCallback _ (SpecialKey s) Down _ _ =
    hPutStrLn stderr $ "Unrecognized key (GLUT): " ++ show s
keyCallback _ _ _ _ _ = return ()

