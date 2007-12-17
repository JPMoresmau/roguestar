{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module RSAGL.Main
    (main,
     displayModel)
    where

import Data.IORef
import System.IO
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.GLU.Errors
import RSAGL.Model
import Models.PlanetRingMoon
import RSAGL.Time
import Control.Monad
import Control.Monad.Trans
import RSAGL.Angle
import System.Exit
import RSAGL.Color
import RSAGL.QualityControl
import RSAGL.Scene
import RSAGL.Animation
import RSAGL.Vector
import RSAGL.RSAGLColors
import RSAGL.CSN
import qualified RSAGL.Affine as Affine
import RSAGL.Matrix

test_quality :: Integer
test_quality = 2^14

testScene :: QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel -> AniM ((),Camera)
testScene qo_planet qo_ring qo_moon =
    do rotation_camera <- rotationM (Vector3D (-1) 1 1) (fromDegrees 5)
       rotation_planet <- rotationM (Vector3D 0 1 0) (fromDegrees 25)
       rotation_moon <- rotationM (Vector3D 0 1 0) (fromDegrees 20)
       orbit_moon <- rotationM (Vector3D 0.1 1 0.1) (fromDegrees (25/30))
       planet_obj <- lift $ getQuality qo_planet test_quality
       ring_obj <- lift $ getQuality qo_ring test_quality
       moon_obj <- lift $ getQuality qo_moon test_quality
       accumulateSceneM Local $ sceneObject ring_obj
       transformM rotation_planet $ accumulateSceneM Local $ sceneObject planet_obj
       let moon_tform = orbit_moon $ Affine.translate (Vector3D 4 0 0) $ rotation_moon $ identityMatrix 4
       transformM (Affine.transform moon_tform) $ accumulateSceneM Local $ sceneObject moon_obj
       accumulateSceneM Local $ lightSource $ DirectionalLight (Vector3D 1 0 0) white blackbody
       accumulateSceneM Local $ lightSource $ DirectionalLight (Vector3D (-1) 0 0) (scaleRGB 0.25 red) blackbody
       return ((),PerspectiveCamera (rotation_camera $ Point3D 2 1 (-2)) (Affine.transform moon_tform $ Point3D 0 0 0) (Vector3D 0 1 0) (fromDegrees 45))

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
       qo_planet <- mkQuality (flip toIntermediateModel planet) $ iterate (*2) 64
       qo_ring <- mkQuality (flip toIntermediateModel ring) $ iterate (*2) 64
       qo_moon <- mkQuality (flip toIntermediateModel moon) $ iterate (*2) 64
       displayCallback $= rsaglDisplayCallback counter (testScene qo_planet qo_ring qo_moon)
       idleCallback $= (Just $ return ())
       addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       mainLoop

rsaglReshapeCallback :: Size -> IO ()
rsaglReshapeCallback (Size width height) = do matrixMode $= Projection
					      loadIdentity
					      viewport $= (Position 0 0,Size width height)
                                              matrixMode $= Modelview 0

rsaglDisplayCallback :: (IORef Integer) -> AniM ((),Camera) -> IO ()
rsaglDisplayCallback counter aniM =
    do loadIdentity
       color (Color4 0.0 0.0 0.0 0.0 :: Color4 Double)
       clear [ColorBuffer]
       the_scene <- liftM snd $ runAniM aniM
       (Size w h) <- GLUT.get windowSize
       sceneToOpenGL (fromIntegral w / fromIntegral h) (0.1,30) the_scene
       swapBuffers
       --modifyIORef counter (+1)
       errs <- (get errors)
       when (not $ null errs) $ print $ show errs
       frames <- readIORef counter
       when (frames >= 1000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
