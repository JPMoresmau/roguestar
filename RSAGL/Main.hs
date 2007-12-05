{-# OPTIONS_GHC -fno-warn-unused-imports -O0 #-}

module RSAGL.Main
    (main,
     displayModel)
    where

import Data.IORef
import System.IO
import Graphics.UI.GLUT as GLUT
import RSAGL.Model
import Models.PlanetRingMoon
import RSAGL.Time
import Control.Monad
import RSAGL.Angle
import System.Exit

--
-- Import your model above and replace "error . . ." with your model below to view it.
--
test_model :: IntermediateModel
test_model = toIntermediateModel 2000 (planet_ring_moon :: Modeling ())

main :: IO ()
main = displayModel

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 16

displayModel :: IO ()
displayModel =
    do _ <- getArgsAndInitialize
       initialWindowSize $= default_window_size
       initialDisplayMode $= display_mode
       window <- createWindow "RSAGL Test Mode"
       reshapeCallback $= Just rsaglReshapeCallback
       counter <- newIORef 0
       displayCallback $= rsaglDisplayCallback counter
       addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       mainLoop

rsaglReshapeCallback :: Size -> IO ()
rsaglReshapeCallback (Size width height) = do matrixMode $= Projection
					      loadIdentity
					      viewport $= (Position 0 0,Size width height)

rsaglDisplayCallback :: (IORef Integer) -> IO ()
rsaglDisplayCallback counter = 
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
       loadIdentity
       color (Color4 0.0 0.0 0.0 0.0 :: Color4 Double)
       clear [ColorBuffer,DepthBuffer]
       cullFace $= Just Front
       depthFunc $= Just Lequal
       depthMask $= Enabled
       lighting $= Enabled
       lightModelAmbient $= (Color4 0 0 0 1)
       (light $ Light 0) $= Enabled
       (ambient $ Light 0) $= (Color4 0 0 0 1.0 :: Color4 Float)
       (GLUT.specular $ Light 0) $= (Color4 1.0 1.0 1.0 1.0 :: Color4 Float)
       (diffuse $ Light 0) $= (Color4 1 1 1 1.0 :: Color4 Float)
       (position $ Light 0) $= (Vertex4 (realToFrac $ 300 * sine secs) 100 (realToFrac $ 100 * cosine secs) 1)
       preservingMatrix $
           do lookAt (Vertex3 (sine (scaleAngle 3 secs)) 1 (2 * cosine (scaleAngle 3 secs))) (Vertex3 0 0 0) (Vector3 0 1 0)
              intermediateModelToOpenGL test_model
       swapBuffers
       modifyIORef counter (+1)
       frames <- readIORef counter
       when (frames >= 1000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
