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
import Control.Monad.Trans
import RSAGL.Angle
import System.Exit
import RSAGL.QualityControl
import RSAGL.Scene
import RSAGL.Animation
import RSAGL.Vector
import RSAGL.RSAGLColors

test_model :: Modeling ()
test_model = planet_ring_moon

test_quality :: Integer
test_quality = 3000

testScene :: QualityCache Integer IntermediateModel -> AniM ((),Camera)
testScene qo = 
    do rotf <- rotationM (Vector3D (-1) 1 1) (fromDegrees 25)
       im <- lift $ getQuality qo test_quality
       accumulateSceneM Local $ sceneObject im
       accumulateSceneM Local $ lightSource $ DirectionalLight (Vector3D 1 1 1) white blackbody
       return ((),rotf $ PerspectiveCamera (Point3D 1 1 (-1)) (Point3D 0 0 0) (Vector3D 0 1 0) (fromDegrees 60))

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
       qo <- mkQuality (flip toIntermediateModel test_model) [100,200..test_quality]
       displayCallback $= rsaglDisplayCallback counter qo
       idleCallback $= (Just $ return ())
       addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       mainLoop

rsaglReshapeCallback :: Size -> IO ()
rsaglReshapeCallback (Size width height) = do matrixMode $= Projection
					      loadIdentity
					      viewport $= (Position 0 0,Size width height)
                                              matrixMode $= Modelview 0

rsaglDisplayCallback :: (IORef Integer) -> QualityCache Integer IntermediateModel -> IO ()
rsaglDisplayCallback counter qo =
    do loadIdentity
       color (Color4 0.0 0.0 0.0 0.0 :: Color4 Double)
       clear [ColorBuffer]
       the_scene <- liftM snd $ runAniM (testScene qo)
       (Size w h) <- GLUT.get windowSize
       sceneToOpenGL (fromIntegral w / fromIntegral h) (0.1,30) the_scene
       swapBuffers
       modifyIORef counter (+1)
       frames <- readIORef counter
       when (frames >= 1000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
