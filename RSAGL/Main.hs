{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
import RSAGL.QualityControl

test_model :: Modeling ()
test_model = planet_ring_moon

quality_level :: Integer
quality_level = 32000

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
       qo <- mkQuality (flip toIntermediateModel test_model) [100,200..quality_level]
       displayCallback $= rsaglDisplayCallback counter qo
       idleCallback $= (Just $ return ())
       addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       mainLoop

rsaglReshapeCallback :: Size -> IO ()
rsaglReshapeCallback (Size width height) = do matrixMode $= Projection
					      loadIdentity
					      viewport $= (Position 0 0,Size width height)

rsaglDisplayCallback :: (IORef Integer) -> QualityCache Integer IntermediateModel -> IO ()
rsaglDisplayCallback counter qo =
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
       (ambient $ Light 0) $= (Color4 0.2 0.2 0.2 1.0 :: Color4 Float)
       (GLUT.specular $ Light 0) $= (Color4 1.0 1.0 1.0 1.0 :: Color4 Float)
       (diffuse $ Light 0) $= (Color4 0.8 0.8 0.8 1.0 :: Color4 Float)
       (position $ Light 0) $= (Vertex4 (realToFrac $ 300 * sine secs) 100 (realToFrac $ 100 * cosine secs) 1)
       preservingMatrix $
           do lookAt (Vertex3 (sine (scaleAngle 3 secs)) 1 (2 * cosine (scaleAngle 3 secs))) (Vertex3 0 0 0) (Vector3 0 1 0)
              intermediateModelToOpenGL =<< getQuality qo quality_level
       swapBuffers
       modifyIORef counter (+1)
       --frames <- readIORef counter
       --when (frames >= 1000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
