module Main
    (main)
    where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import Config
import Processes
import Initialization
import Globals
import Control.Concurrent.STM
import System.Environment

main :: IO ()
main =
    do args <- getArgs
       initGUI
       init_vars <- initialize args
       window <- windowNew
       gl_config <- glConfigNew [GLModeRGB,GLModeDouble,GLModeDepth]
       gl_port <- glDrawingAreaNew gl_config
       let (width,height) = default_window_size
       set window
           [windowTitle := window_name,
            windowDefaultWidth := width,
            windowDefaultHeight := height,
            containerBorderWidth := 0,
            containerChild := gl_port]
       onDestroy window $ atomically $ writeTVar
           (global_should_quit $ init_globals init_vars) True
       widgetShowAll window
       mainGUI

