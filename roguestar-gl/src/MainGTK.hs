module MainGTK
    (mainGTK)
    where

import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Gdk.Events as GdkEvents
import Graphics.UI.Gtk.OpenGL as Gtkglext
import Config
import Processes
import Initialization as Init
import Globals
import Control.Concurrent.STM
import System.Environment
import Graphics.Rendering.OpenGL
import System.IO
import Graphics.UI.GLUT as GLUT
import DrawString
import PrintText
import KeyStroke

mainGTK :: IO ()
mainGTK =
    do prog_name <- getProgName
       args <- getArgs
       initGUI
       GLUT.initialize prog_name args
       mapM_ (hPutStrLn stderr) =<< initGL
       init_vars <- Init.initialize glutDrawString args
       window <- windowNew
       gl_config <- glConfigNew [GLModeRGB,GLModeDouble,GLModeDepth]
       gl_port <- glDrawingAreaNew gl_config
       let (width,height) = default_window_size
       set window
           [Gtk.windowTitle := window_name,
            windowDefaultWidth := width,
            windowDefaultHeight := height,
            containerBorderWidth := 0,
            containerChild := gl_port]
       onDestroy window $ atomically $ writeTVar
           (global_should_quit $ init_globals init_vars) True
       widgetAddEvents window [KeyPressMask]
       onKeyPress window $ \event -> case event of
           Key {} -> do case (GdkEvents.eventKeyName event,eventKeyChar event) of
                            (_,Just c) -> pushInputBuffer
                                              (init_print_text_object init_vars)
                                              (Stroke c)
                            _ -> return ()
                        return True
           _ -> return False
       widgetShowAll window
       sceneLoop init_vars
       timeoutAdd (theDisplayCallback gl_port init_vars >> return True)
                  timer_callback_millis
       mainGUI

theDisplayCallback :: GLDrawingArea -> Initialization -> IO ()
theDisplayCallback draw_area init_vars = withGLDrawingArea draw_area $ \win ->
    do (width, height) <- glDrawableGetSize win
       display (Size (fromIntegral width) (fromIntegral height)) init_vars
       glDrawableSwapBuffers win

