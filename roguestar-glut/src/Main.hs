{-# LANGUAGE Arrows, OverloadedStrings #-}

module Main
    (main)
    where

import System.IO
import Data.Maybe
import Graphics.UI.GLUT as GLUT hiding (initialize)
import Control.Monad
import System.Timeout
import System.Exit
import Config
import Initialization
import Processes
import RSAGL.Scene
import KeyStroke as KeyStroke
import DrawString
import PrintText

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
                WithDepthBuffer,
                DoubleBuffered]

main :: IO ()
main =
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
keyCallback pto (Char ' ') Down _ _ =
    pushInputBuffer pto KeyActivate
keyCallback pto (Char '\n') Down _ _ =
    pushInputBuffer pto KeyActivate
keyCallback pto (Char '\r') Down _ _ =
    pushInputBuffer pto KeyActivate
keyCallback pto (Char '\ESC') Down _ _ =
    pushInputBuffer pto KeyEscape
keyCallback pto (Char '&') Down _ _ =
    pushInputBuffer pto KeyAmpersand
keyCallback pto (Char ';') Down _ _ =
    pushInputBuffer pto KeySemicolon
keyCallback pto (Char '\t') Down _ _ =
    pushInputBuffer pto KeyTab
keyCallback pto (Char '1') Down _ _ =
    pushInputBuffer pto NumPad1
keyCallback pto (Char '2') Down _ _ =
    pushInputBuffer pto NumPad2
keyCallback pto (Char '3') Down _ _ =
    pushInputBuffer pto NumPad3
keyCallback pto (Char '4') Down _ _ =
    pushInputBuffer pto NumPad4
keyCallback pto (Char '5') Down _ _ =
    pushInputBuffer pto NumPad5
keyCallback pto (Char '6') Down _ _ =
    pushInputBuffer pto NumPad6
keyCallback pto (Char '7') Down _ _ =
    pushInputBuffer pto NumPad7
keyCallback pto (Char '8') Down _ _ =
    pushInputBuffer pto NumPad8
keyCallback pto (Char '9') Down _ _ =
    pushInputBuffer pto NumPad9
keyCallback pto (Char '0') Down _ _ =
    pushInputBuffer pto NumPad0
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

