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
import RSAGL.Bottleneck
import RSAGL.QualityControl
import RSAGL.Scene
import RSAGL.FRP
import RSAGL.Animation
import RSAGL.AnimationExtras
import Control.Arrow
import RSAGL.Vector
import RSAGL.RSAGLColors
import RSAGL.CoordinateSystems
import qualified RSAGL.Affine as Affine
import RSAGL.Matrix
import RSAGL.Interpolation
import Debug.Trace
import RSAGL.ModelingExtras
import RSAGL.WrappedAffine
import RSAGL.InverseKinematics

test_quality :: Integer
test_quality = 2^14
--test_quality = 64

moon_orbital_animation :: AniA i o (IO IntermediateModel) (CSN Point3D)
moon_orbital_animation =
    accelerationModel (perSecond 60)
                      (Point3D (-6) 0 0,perSecond $ Vector3D 0.0 0.14 0.18)
                      (arr $ const $ inverseSquareLaw 1.0 origin_point_3d)
                      (proc (_,im) -> do rotateA (Vector3D 0 1 0) (perSecond $ fromDegrees 20) accumulateSceneA -< (Infinite,sceneObject im)
                                         exportA -< origin_point_3d)

walking_orb_animation :: QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel ->
                         QualityCache Integer IntermediateModel -> QualityCache Integer IntermediateModel ->
                         IO (AniA i o () ())
walking_orb_animation qo_orb qo_glow_orb qo_orb_upper_leg qo_orb_lower_leg =
    do let upper_leg_anim = proc () -> accumulateSceneA -< (Local,sceneObject $ getQuality qo_orb_upper_leg 50)
       let lower_leg_anim = proc () -> accumulateSceneA -< (Local,sceneObject $ getQuality qo_orb_lower_leg 50)
       let orb_legs = legs $ rotationGroup (Vector3D 0 1 0) 7 $
                             leg (Vector3D 0 1 1) (Point3D 0 0.5 0.5) 2 (Point3D 0 0 1.8) $ legAnimation upper_leg_anim lower_leg_anim
       return $ proc () ->
           do accumulateSceneA -< (Local,sceneObject $ getQuality qo_orb test_quality)
              transformA pointAtCameraA -< (Affine.translate (Vector3D 0 1.05 0),(Local,getQuality qo_glow_orb test_quality))
              accumulateSceneA -< (Local,lightSource $ PointLight (Point3D 0 0 0)
                                                                  (measure (Point3D 0 0 0) (Point3D 0 6 0))
                                                                  (scaleRGB 0.5 white) blackbody)
              orb_legs -< ()
              returnA -< ()

testScene :: IO (AniM ((),Camera))
testScene = 
    do bottleneck <- newBottleneck
       let newQO im = newQuality bottleneck parIntermediateModel (flip toIntermediateModel im) $ iterate (*2) 64
       putStrLn "loading planet..."
       qo_planet <- newQO planet
       putStrLn "loading ring..."
       qo_ring <- newQO ring
       putStrLn "loading moon..."
       qo_moon <- newQO moon
       putStrLn "loading ground..."
       qo_ground <- newQO ground
       putStrLn "loading monolith..."
       qo_monolith <- newQO monolith
       putStrLn "loading station..."
       qo_station <- newQO station
       putStrLn "loading orb..."
       qo_orb <- newQO orb
       putStrLn "loading glow_orb..."
       qo_glow_orb <- newQO glow_orb
       putStrLn "loading orb_upper_leg..."
       qo_orb_upper_leg <- newQO orb_upper_leg
       putStrLn "loading orb_lower_leg..."
       qo_orb_lower_leg <- newQO orb_lower_leg
       putStrLn "done."
       walking_orb_animation_arrow <- walking_orb_animation qo_orb qo_glow_orb qo_orb_upper_leg qo_orb_lower_leg
       ao_walking_orb <- newAnimationObjectA [walking_orb_animation_arrow]
       ao_moon_orbit <- newAnimationObjectA [arr (\x -> [x]) <<< moon_orbital_animation]
       return $ 
           do rotation_planet <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 25)
              rotation_station <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 5)
              rotation_camera <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 3)
              rotation_orb <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 7)
              accumulateSceneM Local $ sceneObject $ getQuality qo_ground test_quality
              accumulateSceneM Local $ sceneObject $ getQuality qo_monolith test_quality
              transformM (Affine.translate (Vector3D 0 1 (-4)) . Affine.rotate (Vector3D 1 0 0) (fromDegrees 90) . rotation_station) $ 
                  accumulateSceneM Infinite $ sceneObject $ getQuality qo_station test_quality
              transformM (rotation_orb . Affine.translate (Vector3D (4) 0 0)) $
                  do runAnimationObject ao_walking_orb ()
              transformM (Affine.translate (Vector3D 0 1 6)) $ 
                  do transformM rotation_planet $ accumulateSceneM Infinite $ sceneObject $ getQuality qo_planet test_quality
                     accumulateSceneM Infinite $ lightSource $ DirectionalLight (vectorNormalize $ Vector3D 1 (-1) (-1)) white blackbody
                     accumulateSceneM Infinite $ lightSource $ DirectionalLight (vectorNormalize $ Vector3D (-1) 1 1) (scaleRGB 0.5 red) blackbody
                     accumulateSceneM Infinite $ sceneObject $ getQuality qo_ring test_quality
                     runAnimationObject ao_moon_orbit $ getQuality qo_moon test_quality
              return ((),PerspectiveCamera (transformation rotation_camera $ Point3D 1 2 (-8))
                                           (Point3D 0 2.5 2)
                                           (Vector3D 0 1 0)
                                           (fromDegrees 45))

main :: IO ()
main = displayModel

default_window_size :: Size
default_window_size = Size 800 600

display_mode :: [DisplayMode]
display_mode = [RGBAMode,
		WithDepthBuffer,
		DoubleBuffered]

timer_callback_millis :: Int
timer_callback_millis = 5

displayModel :: IO ()
displayModel =
    do _ <- getArgsAndInitialize
       initialWindowSize $= default_window_size
       initialDisplayMode $= display_mode
       window <- createWindow "RSAGL Test Mode"
       reshapeCallback $= Just rsaglReshapeCallback
       counter <- newIORef 0
       testSceneCallback <- testScene
       displayCallback $= rsaglDisplayCallback counter (testSceneCallback)
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
       modifyIORef counter (+1)
       errs <- (get errors)
       when (not $ null errs) $ print $ show errs
       frames <- readIORef counter
       when (frames `mod` 200 == 0) $ putStrLn $ "frames: " ++ show frames
       when (frames >= 4000) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window
