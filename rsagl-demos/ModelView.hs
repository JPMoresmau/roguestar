{-# LANGUAGE Arrows #-}

module Main
    (main,
     displayModel)
    where

import Data.IORef
import Graphics.UI.GLUT as GLUT hiding (specular,scale,translate,rotate)
import System.Random
import RSAGL.Modeling
import RSAGL.FRP
import RSAGL.Animation
import RSAGL.Scene
import RSAGL.Math
import RSAGL.RayTrace.RayTrace as RT
import RSAGL.Extras.Sky
import RSAGL.Math.CurveExtras
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import Control.Monad
import System.Exit
import Control.Arrow
import qualified RSAGL.Math.Affine as Affine

test_quality :: Integer
test_quality = 2^10
--test_quality = 64

{- Until program termination. -}
number_of_frames :: Integer
number_of_frames = 6000
--number_of_frames = 60

moon_orbital_animation :: (FRPModel m) => FRP e (SimpleSwitch k () (SceneAccumulator IO) i o m) (IO BakedModel) (CSN Point3D)
moon_orbital_animation =
    accelerationModel (perSecond 60)
                      (Point3D (-6) 0 0,perSecond $ Vector3D 0.0 0.14 0.18)
                      (arr $ const $ inverseSquareLaw 1.0 origin_point_3d)
                      (proc (_,im) -> do rotateA (Vector3D 0 1 0) (perSecond $ fromDegrees 20) accumulateSceneA -< (std_scene_layer_infinite+2,sceneObject im)
                                         exportA -< origin_point_3d)

walking_orb_animation :: (FRPModel m) =>
                         BakedModel ->
                         BakedModel ->
                         BakedModel ->
                         BakedModel ->
                         FRP e
                             (SimpleSwitch k () (SceneAccumulator IO) i o m)
                             () ()
walking_orb_animation qo_orb qo_glow_orb qo_orb_upper_leg qo_orb_lower_leg =
    proc () ->
        do accumulateSceneA -< (std_scene_layer_local,
                                sceneObject $ return qo_orb)
           transformA pointAtCameraA -< (Affine $
               Affine.translate (Vector3D 0 1.05 0),
               (std_scene_layer_local,return qo_glow_orb))
           accumulateSceneA -< (std_scene_layer_local,
               lightSource $ PointLight (Point3D 0 0 0)
                                        (measure (Point3D 0 0 0) (Point3D 0 6 0))
                                        (scalarMultiply 0.5 white)
                                        blackbody)
           orb_legs -< ()
           returnA -< ()
  where upper_leg_anim = proc () -> accumulateSceneA -<
                             (std_scene_layer_local,
                              sceneObject $ return qo_orb_upper_leg)
        lower_leg_anim = proc () -> accumulateSceneA -<
                             (std_scene_layer_local,
                              sceneObject $ return qo_orb_lower_leg)
        orb_legs = legs $
            rotationGroup (Vector3D 0 1 0) 25 $
                leg (Vector3D 0 1 1)
                    (Point3D 0 0.5 0.5)
                    2
                    (Point3D 0 0 1.8) $
                        jointAnimation upper_leg_anim lower_leg_anim

testScene :: IO (AniM ((),Camera))
testScene =
    do let newQO :: Integer -> Modeling () -> IO BakedModel
           newQO v im = bakeModel $ buildIntermediateModel v im
       putStrLn "building planet..."
       qo_planet <- newQO test_quality planet
       putStrLn "building ring..."
       qo_ring <- newQO test_quality ring
       putStrLn "building moon..."
       qo_moon <- newQO test_quality moon
       putStrLn "building ground..."
       qo_ground <- newQO test_quality ground
       putStrLn "building monolith..."
       qo_monolith <- newQO 20 monolith
       putStrLn "building station..."
       qo_station <- newQO test_quality station
       putStrLn "building orb..."
       qo_orb <- newQO test_quality orb
       putStrLn "building glow_orb..."
       qo_glow_orb <- newQO test_quality glow_orb
       putStrLn "building orb_upper_leg..."
       qo_orb_upper_leg <- newQO 50 orb_upper_leg
       putStrLn "building orb_lower_leg..."
       qo_orb_lower_leg <- newQO 50 orb_lower_leg
       putStrLn "building sky..."
       qo_sky <- newQO test_quality sky
       putStrLn "done."
       let walking_orb_animation_arrow = walking_orb_animation
                                             qo_orb
                                             qo_glow_orb
                                             qo_orb_upper_leg
                                             qo_orb_lower_leg
       ao_walking_orb <- newAnimationObjectA (arr (map snd) <<< frpContext nullaryThreadIdentity [((),walking_orb_animation_arrow)])
       ao_moon_orbit <- newAnimationObjectA (arr (map snd) <<< frpContext nullaryThreadIdentity [((),moon_orbital_animation)])
       return $
           do rotation_planet <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 25)
              rotation_station <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 5)
              rotation_camera <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 3)
              rotation_orb <- rotationM (Vector3D 0 1 0) (perSecond $ fromDegrees 7)
              accumulateSceneM std_scene_layer_local $ sceneObject $ return qo_ground
              accumulateSceneM std_scene_layer_local $ sceneObject $ return qo_monolith
              transformM (affineOf $ Affine.translate $ Vector3D 0 (-0.01) 0) $
                  do accumulateSceneM (std_scene_layer_infinite+1) $
                         sceneObject $ return qo_sky
                     accumulateSceneM (std_scene_layer_infinite+1) $
                         lightSource $ skylight (Vector3D 0 1 0)
                                                (scalarMultiply 0.25 periwinkle)
                     accumulateSceneM (std_scene_layer_infinite+1) $
                         sceneObject $ return qo_ground
              transformM (affineOf $ Affine.translate (Vector3D 0 1 (-4)) .
                                     Affine.rotate (Vector3D 1 0 0) (fromDegrees 90) . 
                                     rotation_station) $
                  accumulateSceneM std_scene_layer_infinite $
                      sceneObject $ return qo_station
              _ <- transformM (affineOf $
                       rotation_orb . Affine.translate (Vector3D (4) 0 0)) $
                           runAnimationObject ao_walking_orb ()
              _ <- transformM (affineOf $ Affine.translate (Vector3D 0 1 6)) $
                  do transformM (affineOf rotation_planet) $
                         accumulateSceneM (std_scene_layer_infinite+2) $
		             sceneObject $ return qo_planet
                     accumulateSceneM (std_scene_layer_infinite+2) $
		         lightSource $ DirectionalLight (vectorNormalize $
                             Vector3D 1 (-1) (-1)) white blackbody
                     accumulateSceneM (std_scene_layer_infinite+2) $
		         lightSource $ DirectionalLight (vectorNormalize $
                             Vector3D (-1) 1 1) (scalarMultiply 0.5 red) blackbody
                     accumulateSceneM (std_scene_layer_infinite+2) $
		         sceneObject $ return qo_ring
                     runAnimationObject ao_moon_orbit $ return qo_moon
              return ((),PerspectiveCamera
                             (transformation rotation_camera $ Point3D 1 2 (-8))
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
       color (Color4 0.0 0.0 0.0 0.0 :: Color4 GLdouble)
       clear [ColorBuffer]
       the_scene <- liftM snd $ runAniM (liftM (second stdSceneLayerInfo) aniM)
       (Size w h) <- GLUT.get windowSize
       sceneToOpenGL (fromIntegral w / fromIntegral h) (0.3,300) the_scene
       swapBuffers
       modifyIORef counter (+1)
       errs <- (get errors)
       when (not $ null errs) $ print $ show errs
       frames <- readIORef counter
       when (frames `mod` 200 == 0) $ putStrLn $ "frames: " ++ show frames
       when (frames >= number_of_frames) $ exitWith ExitSuccess

rsaglTimerCallback :: Window -> IO ()
rsaglTimerCallback window = 
    do addTimerCallback timer_callback_millis (rsaglTimerCallback window)
       postRedisplay $ Just window

ring :: Modeling ()
ring = model $ do openDisc origin_point_3d (Vector3D 0 1 0) 0.75 1.0
                  material $
		      do transparent $ pure $ alpha 0.25 $ transformColor purple
                         specular 2 $ pure purple
                  bumps $ waves 0.2 0.01
                  twoSided True

planet :: Modeling ()
planet = model $ 
    do RSAGL.Modeling.sphere (Point3D 0 0 0) 0.65
       deform $ constrain (\(SurfaceVertex3D (Point3D x y z) _) -> x > 0 && y > 0 && z > 0) $ 
           RT.shadowDeform (Vector3D (-1) (-1) (-1)) (map (RT.plane (Point3D 0 0 0)) [Vector3D 1 0 0,Vector3D 0 1 0,Vector3D 0 0 1])
       let land_vs_water land water = pattern (cloudy 26 0.4) [(0,water),(0.5,water),(0.51,land),(1,land)]
       let grass_and_mountains = pattern (cloudy 81 0.25) [(0.4,pattern (cloudy 99 0.1) [(0.0,pure brown),(1.0,pure grey)]),(0.5,pure forest_green)]
       let land_and_water = land_vs_water grass_and_mountains (pure blue)
       let cities bright dark = land_vs_water (pattern (cloudy 5 0.1) [(0.0,bright),(0.5,dark)]) (dark)
       let planet_surface = pattern (gradient (Point3D 0 0 0) (Vector3D 0 0.65 0)) 
                                    [(-0.9,pure white),(-0.85,land_and_water),(0.85,land_and_water),(0.9,pure white)]
       let planet_interior inner_core outer_core crust = pattern (spherical (Point3D 0 0 0) 0.65) 
                  [(0.0,inner_core),(0.25,inner_core),(0.5,outer_core),(0.95,outer_core),(1.0,crust)]
       material $
           do pigment $ planet_interior (pure blackbody) (pure blackbody) $ cities (pure black) planet_surface
              emissive $ planet_interior (pure yellow) (pure red) $ cities (pure $ scalarMultiply 0.2 white) (pure blackbody)
              specular 20 $ planet_interior (pure blackbody) (pure blackbody) $ land_vs_water (pure blackbody) (pure white)

moon :: Modeling ()
moon = model $ 
    do RSAGL.Modeling.sphere (Point3D 0 0 0) 0.2
       material $ pigment $ pattern (cloudy 8 0.05) [(0.0,pure grey),(1.0,pure white)]
       regenerateNormals

monolith :: Modeling ()
monolith = model $
    do smoothbox 0.1 (Point3D 4 9 1) (Point3D (-4) (-9) (-1))
       affine (translate $ Vector3D 0 9 0)
       affine (scale' 0.20)
       material $
           do pigment $ pure blackbody
              specular 100 $ pure white

ground :: Modeling ()
ground = model $
    do closedDisc (Point3D 0 (-0.1) 0) (Vector3D 0 1 0) 30
       regenerateNormals
       material $ pigment $ pattern (cloudy 27 1.0) [(0.0,pure brown),(1.0,pure forest_green)]
       affine $ translate (Vector3D 0 (-0.1) 0)

station :: Modeling ()
station = model $
    do model $ 
         do torus 0.5 0.1
            openCone (Point3D (-0.5) 0 0,0.02) (Point3D 0.5 0 0,0.02)
            openCone (Point3D 0 0 (-0.5),0.02) (Point3D 0 0 0.5,0.02)
            closedCone (Point3D 0 0.2 0,0.2) (Point3D 0 (-0.2) 0,0.2)
            material $ 
	        do pigment $ pure grey
                   specular 100 $ pure grey
       model $ 
         do box (Point3D (-0.15) 0.19 (-0.05)) (Point3D 0.15 0.21 0.05)
            material $ emissive $ pure white
       sequence_ $ dropRandomElements 30 (mkStdGen 19) $ concatMap (rotationGroup (Vector3D 0 1 0) 40) $ 
         [window_box,
          transformAbout (Point3D 0.5 0 0) (rotateZ $ fromDegrees 25) window_box,
          transformAbout (Point3D 0.5 0 0) (rotateZ $ fromDegrees (-25)) window_box,
          transformAbout (Point3D 0.5 0 0) (rotateZ $ fromDegrees 50) window_box,
          transformAbout (Point3D 0.5 0 0) (rotateZ $ fromDegrees (-50)) window_box]
            where window_box = model $
                      do quadralateral (Point3D 0.51 (-0.105) 0.03) (Point3D 0.49 (-0.105) 0.03)
                                       (Point3D 0.49 (-0.105) (-0.03)) (Point3D 0.51 (-0.105) (-0.03))
                         quadralateral (Point3D 0.51 0.105 (-0.03)) (Point3D 0.49 0.105 (-0.03))
                                       (Point3D 0.49 0.105 0.03) (Point3D 0.51 0.105 0.03)
                         material $
			     do pigment $ pure black
                                emissive $ pure white
                         tesselationHintComplexity 0
                         fixed (3,3)

orb :: Modeling ()
orb = model $
    do sor $ linearInterpolation $ points2d
               [(-0.001,0.4),
                (0.5,0.45),
                (0.5,0.4),
                (0.6,0.4),
                (0.6,0.6),
                (0.5,0.6),
                (0.5,0.55),
                (-0.001,0.6)]
       sequence_ $ rotationGroup (Vector3D 0 1 0) 5 $
           tube $ zipCurve (,) (pure 0.05) $ smoothCurve 3 0.4 $ loopedLinearInterpolation $ points3d
               [(0.4,0.2,0.4),
                (0.4,0.8,0.8),
                (-0.4,0.8,0.8),
                (-0.4,0.2,0.4)]
       regularPrism (Point3D 0 0.5 0,0.5) (Point3D 0 1.0 0,-0.001) 4
       material $
           do pigment $ pure yellow
              specular 64 $ pure grey

glow_orb :: Modeling ()
glow_orb = translate (Vector3D 0 1 0) $
    do closedDisc (Point3D 0 0 0) (Vector3D 0 1 0) 1
       material $ emissive $ pattern (spherical (Point3D 0 0 0) 1) [(0.0,pure $ scalarMultiply 1.5 white),(0.25,pure white),(0.95,pure blackbody)]

orb_upper_leg :: Modeling ()
orb_upper_leg = model $
    do tube $ zipCurve (,) (pure 0.05) $ linearInterpolation [Point3D 0 0 0,Point3D 0 0.1 0.5,Point3D 0 0 1]
       RSAGL.Modeling.sphere (Point3D 0 0 1) 0.05
       material $
           do pigment $ pure yellow
              specular 64 $ pure grey

orb_lower_leg :: Modeling ()
orb_lower_leg = model $
    do openCone (Point3D 0 0 0,0.05) (Point3D 0 0 1,0)
       material $ 
           do pigment $ pure yellow
              specular 64 $ pure grey

sky :: Modeling ()
sky = model $
    do skyHemisphere (Point3D 0 0 0) (Vector3D 0 1 0) 1.0
       affine $ scale $ Vector3D 5 1 5
       material $ atmosphereScatteringMaterial earth_atmosphere [(rotate (Vector3D 0 0 1) (fromTimeOfDayHMS 0 15 0) $ Vector3D 1 0 0,grayscale 1)] (dynamicSkyFilter 0.2 1.0)

