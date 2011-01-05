module Main
    (main)
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
import Data.Char

main :: IO ()
main =
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
       onDelete window $ const $
           do atomically $ writeTVar
                  (global_should_quit $ init_globals init_vars)
                  True
              return True
       widgetAddEvents window [KeyPressMask]
       onKeyPress window $ \event -> case event of
           Key {} -> do pushInputBuffer (init_print_text_object init_vars) $
                            case (map toUpper $ GdkEvents.eventKeyName event,
                                     eventKeyChar event) of
                                 (_,Just ' ') -> KeyActivate
                                 (_,Just '&') -> KeyAmpersand
                                 (_,Just ';') -> KeySemicolon
                                 (_,Just '\n') -> KeyActivate
                                 (_,Just '\r') -> KeyActivate
                                 (_,Just '\ESC') -> KeyEscape
                                 (_,Just '\t') -> KeyTab
                                 ("UP",_) -> KeyStroke.KeyUp
                                 ("DOWN",_) -> KeyStroke.KeyDown
                                 ("LEFT",_) -> KeyStroke.KeyLeft
                                 ("RIGHT",_) -> KeyStroke.KeyRight
                                 ("RETURN",_) -> KeyActivate
                                 ("KP_ENTER",_) -> KeyActivate
                                 ("ISO_ENTER",_) -> KeyActivate
                                 ("3270_ENTER",_) -> KeyActivate
                                 ("KP_SPACE",_) -> KeyActivate
                                 ("SPACE",_) -> KeyActivate
                                 ("ESCAPE",_) -> KeyEscape
                                 ("DELETE",_) -> KeyEscape
                                 ("KP_DELETE",_) -> KeyEscape
                                 ("BACKSPACE",_) -> KeyEscape
                                 ("TAB",_) -> KeyTab
                                 ("KP_TAB",_) -> KeyTab
                                 ("ISO_LEFT_TAB",_) -> KeyTab
                                 ("KP_1",_) -> NumPad1
                                 ("KP_2",_) -> NumPad2
                                 ("KP_3",_) -> NumPad3
                                 ("KP_4",_) -> NumPad4
                                 ("KP_5",_) -> NumPad5
                                 ("KP_6",_) -> NumPad6
                                 ("KP_7",_) -> NumPad7
                                 ("KP_8",_) -> NumPad8
                                 ("KP_9",_) -> NumPad9
                                 ("KP_0",_) -> NumPad0
                                 (_,Just c) -> Stroke c
                                 _ -> KeyIgnored
                        return True
           _ -> return False
       timeoutAdd (theDisplayCallback gl_port init_vars >> return True)
                  timer_callback_millis
       widgetShowAll window
       sceneLoop init_vars
       mainGUI

theDisplayCallback :: GLDrawingArea -> Initialization -> IO ()
theDisplayCallback draw_area init_vars = withGLDrawingArea draw_area $ \win ->
    do watchQuit init_vars
       (width, height) <- glDrawableGetSize win
       display (Size (fromIntegral width) (fromIntegral height)) init_vars
       glDrawableSwapBuffers win


