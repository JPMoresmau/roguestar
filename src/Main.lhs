\section{Main}

\begin{code}
{-# LANGUAGE Arrows #-}

module Main
    (main)
    where

import System.IO
import PrintText
--import Quality
import Data.IORef
import Data.Maybe
import Data.List
import Graphics.UI.GLUT
import Control.Monad
import Actions
import Keymaps
import DefaultKeymap
import RenderingControl
import Driver
--import Models.LibraryData
--import Models.Library
--import Camera
import Animation
import RSAGL.Scene
import RSAGL.Vector
import RSAGL.Angle
import Control.Arrow
import Models.Library
import System.Timeout
import System.Exit

roguestar_client_version :: String
roguestar_client_version = "0.2"

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

--qualityFromArgs :: [String] -> Quality
--qualityFromArgs [] = Good
--qualityFromArgs ("quality-bad":_) = Bad
--qualityFromArgs ("quality-poor":_) = Poor
--qualityFromArgs ("quality-good":_) = Good
--qualityFromArgs ("quality-super":_) = Super
--qualityFromArgs args = qualityFromArgs $ tail args

main :: IO ()
main = do (_,_) <- getArgsAndInitialize
	  driver_object <- newDriverObject
	  print_text_object <- newPrintTextObject
	  animation_object <- newRoguestarAnimationObject mainAnimationLoop
	  lib <- newLibrary
	  initialWindowSize $= default_window_size
	  initialDisplayMode $= display_mode
	  window <- createWindow $ "RogueStar GL " ++ roguestar_client_version 
	  reshapeCallback $= Just roguestarReshapeCallback
	  displayCallback $= roguestarDisplayCallback lib driver_object print_text_object animation_object
	  keyboardMouseCallback $= (Just $ keyCallback print_text_object)
	  addTimerCallback timer_callback_millis (roguestarTimerCallback driver_object print_text_object window)
	  mainLoop

roguestarReshapeCallback :: Size -> IO ()
roguestarReshapeCallback (Size width height) = 
    do matrixMode $= Projection
       loadIdentity
       viewport $= (Position 0 0,Size width height)

roguestarDisplayCallback :: Library -> DriverObject -> PrintTextObject -> RoguestarAnimationObject -> IO ()
roguestarDisplayCallback lib driver_object print_text_object animation_object =
  do result <- timeout 20000000 $
         do color (Color4 0 0 0 0 :: Color4 Float)
            clear [ColorBuffer]
            scene <- runRoguestarAnimationObject lib driver_object print_text_object animation_object 
            (Size width height) <- get windowSize
            sceneToOpenGL (fromIntegral width / fromIntegral height) (0.1,80.0) scene
            renderText print_text_object 
            swapBuffers
     if isNothing result
         then do hPutStrLn stderr "roguestar-gl: aborting due to stalled display callback (timed out after 20 seconds)"
	         exitWith $ ExitFailure 1
	 else return ()

roguestarTimerCallback :: DriverObject -> PrintTextObject -> Window -> IO ()
roguestarTimerCallback driver_object print_text_object window = 
    do result <- timeout 20000000 $
        do addTimerCallback timer_callback_millis $ roguestarTimerCallback driver_object print_text_object window
           driverRead driver_object
           postRedisplay $ Just window
           maybeExecuteKeymappedAction driver_object print_text_object default_keymap
       if isNothing result
           then do hPutStrLn stderr "roguestar-gl: aborting due to stalled timer callback (timed out after 20 seconds)"
	           exitWith $ ExitFailure 1
           else return ()

maybeExecuteKeymappedAction :: DriverObject -> PrintTextObject -> Keymap -> IO ()
maybeExecuteKeymappedAction driver_object print_text_object keymap =
    do let action_input = ActionInput driver_object print_text_object
       buffer_contents <- getInputBuffer print_text_object
       actions <- keysToActionNames action_input keymap buffer_contents
       worked <- takeUserInputAction action_input actions
       if worked
           then clearInputBuffer print_text_object
	   else setInputBuffer print_text_object =<< filterKeySequence action_input default_keymap buffer_contents
\end{code}
