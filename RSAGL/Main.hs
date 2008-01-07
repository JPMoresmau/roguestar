{-# OPTIONS_GHC -fno-warn-unused-imports -farrows #-}

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
import RSAGL.FRP
import RSAGL.Animation
import RSAGL.AnimationExtras
import Control.Arrow
import RSAGL.Vector
import RSAGL.RSAGLColors
import RSAGL.CSN
import qualified RSAGL.Affine as Affine
import RSAGL.Matrix
import RSAGL.Interpolation
import Debug.Trace

test_quality :: Integer
test_quality = 2^13

moon_orbital_animation :: AniA i o IntermediateModel (CSN Point3D)
moon_orbital_animation =
    accelerationModel (perSecond 60)
                      (Point3D (-6) 0 0,perSecond $ Vector3D 0.0 0.14 0.18)
                      (arr $ const $ inverseSquareLaw 1.0 origin_point_3d)
                      (proc (_,im) -> do rotateA (Vector3D 0 1 0) (perSecond $ fromDegrees 20) accumulateSceneA -< (Local,sceneObject im)
                                         exportA -< origin_point_3d)

testScene :: QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel -> 
             AnimationObject IntermediateModel [CSN Point3D] -> AniM ((),Camera)
testScene qo_planet qo_ring qo_moon ao_moon_orbit =
    do rotation_camera <- rotationM (Vector3D 1 1 1) (perSecond $ fromDegrees 0.5)
       rotation_planet <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 25)
       planet_obj <- liftIO $ getQuality qo_planet test_quality
       ring_obj <- liftIO $ getQuality qo_ring test_quality
       moon_obj <- liftIO $ getQuality qo_moon test_quality
       moon_position <- (liftM head $ runAnimationObject ao_moon_orbit moon_obj) >>= importM
       transformM rotation_planet $ accumulateSceneM Local $ sceneObject planet_obj
       accumulateSceneM Local $ lightSource $ DirectionalLight (vectorNormalize $ Vector3D 1 0.5 0) white blackbody
       accumulateSceneM Local $ lightSource $ DirectionalLight (vectorNormalize $ Vector3D (-1) (-0.5) 0) (scaleRGB 0.25 red) blackbody
       accumulateSceneM Local $ sceneObject ring_obj
       return ((),PerspectiveCamera (transformation rotation_camera $ Point3D 6.5 0 (-8)) 
                                    (lerp 0.5 (moon_position,origin_point_3d)) 
                                    (Vector3D 0 1 0) (fromDegrees 45))

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
       ao_moon_orbit <- newAnimationObjectA [arr (\x -> [x]) <<< moon_orbital_animation]
       displayCallback $= rsaglDisplayCallback counter (testScene qo_planet qo_ring qo_moon ao_moon_orbit)
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
       fog $= Enabled
       fogMode $= Linear 0.0 25.0
       fogColor $= (Color4 0 0 0 0 :: Color4 GLfloat)
       sceneToOpenGL (fromIntegral w / fromIntegral h) (0.1,30) the_scene
       swapBuffers
       modifyIORef counter (+1)
       errs <- (get errors)
       when (not $ null errs) $ print $ show errs
       frames <- readIORef counter
       when (frames >= 10000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
