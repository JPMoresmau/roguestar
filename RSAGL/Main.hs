{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module RSAGL.Main
    (main,
     displayModel)
    where

import System.IO
import Graphics.UI.GLUT
import RSAGL.Model
import Models.SineWave
import RSAGL.Time
import Control.Monad
import RSAGL.Angle

--
-- Import your model above and replace "undefined" with your model below to view it.
--
model :: Model
model = error "Please define a model in RSAGL.Main.model"
--model = sine_wave

main :: IO ()
main = displayModel

display_function :: IO ()
display_function = toOpenGL $ scaleModel 0.4 model

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 30

displayModel :: IO ()
displayModel =
    do _ <- getArgsAndInitialize
       initialWindowSize $= default_window_size
       initialDisplayMode $= display_mode
       window <- createWindow "RSAGL TEST MODE"
       reshapeCallback $= Just rsaglReshapeCallback
       displayCallback $= rsaglDisplayCallback
       addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       mainLoop

rsaglReshapeCallback :: Size -> IO ()
rsaglReshapeCallback (Size width height) = do matrixMode $= Projection
					      loadIdentity
					      viewport $= (Position 0 0,Size width height)

rsaglDisplayCallback :: IO ()
rsaglDisplayCallback = 
    do secs <- liftM (fromRadians . (*(2*pi))) $ cycleSeconds 60
       matrixMode $= Projection
       rescaleNormal $= Enabled
       loadIdentity
       (Size width height) <- get windowSize
       perspective (60) 
                   ((fromInteger $ toInteger width)/(fromInteger $ toInteger height)) 
                   (0.1) 
                   (100)
       matrixMode $= Modelview 0
       color (Color4 0.0 0.0 0.0 0.0 :: Color4 Double)
       clear [ColorBuffer,DepthBuffer]
       depthFunc $= Just Lequal
       depthMask $= Enabled
       lighting $= Enabled
       lightModelAmbient $= (Color4 0 0 0 1)
       (light $ Light 0) $= Enabled
       (ambient $ Light 0) $= (Color4 0.2 0.0 0.0 1.0 :: Color4 Float)
       (specular $ Light 0) $= (Color4 1.0 1.0 1.0 1.0 :: Color4 Float)
       (diffuse $ Light 0) $= (Color4 1.0 1.0 1.0 1.0 :: Color4 Float)
       (position $ Light 0) $= (Vertex4 1 1 1 0)
       preservingMatrix $
           do lookAt (Vertex3 (2 * sine (scaleAngle 7 secs)) (sine secs) (2 * cosine (scaleAngle 5 secs))) (Vertex3 0 0 0) (Vector3 0 1 0)
              display_function
	      swapBuffers

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
