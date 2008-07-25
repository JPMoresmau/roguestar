module Models.PlanetRingMoon
    (planet,ring,moon,ground,monolith,station,orb,glow_orb,orb_upper_leg,orb_lower_leg,sky)
    where

import RSAGL.Model
import RSAGL.Vector
import RSAGL.ModelingExtras
import qualified RSAGL.RayTrace as RT
import RSAGL.Affine
import RSAGL.Auxiliary
import System.Random
import RSAGL.Angle
import RSAGL.CurveExtras
import RSAGL.Curve

ring :: Modeling ()
ring = model $ do openDisc origin_point_3d (Vector3D 0 1 0) 0.75 1.0
                  material $
		      do transparent $ pure $ alpha 0.25 purple
                         specular 2 $ pure purple
                  bumps $ waves 0.2 0.01
                  twoSided True

planet :: Modeling ()
planet = model $ 
    do sphere (Point3D 0 0 0) 0.65
       deform $ constrain (\(SurfaceVertex3D (Point3D x y z) _) -> x > 0 && y > 0 && z > 0) $ 
           RT.shadowDeform (Vector3D (-1) (-1) (-1)) (map (RT.plane (Point3D 0 0 0)) [Vector3D 1 0 0,Vector3D 0 1 0,Vector3D 0 0 1])
       let land_vs_water land water = pattern (cloudy 26 0.4) [(0,water),(0.5,water),(0.51,land),(1,land)]
       let grass_and_mountains = pattern (cloudy 81 0.25) [(0.4,pattern (cloudy 99 0.1) [(0.0,pure brown),(1.0,pure slate_gray)]),(0.5,pure forest_green)]
       let land_and_water = land_vs_water grass_and_mountains (pure blue)
       let cities bright dark = land_vs_water (pattern (cloudy 5 0.1) [(0.0,bright),(0.5,dark)]) (dark)
       let planet_surface = pattern (gradient (Point3D 0 0 0) (Vector3D 0 0.65 0)) 
                                    [(-0.9,pure white),(-0.85,land_and_water),(0.85,land_and_water),(0.9,pure white)]
       let planet_interior inner_core outer_core crust = pattern (spherical (Point3D 0 0 0) 0.65) 
                  [(0.0,inner_core),(0.25,inner_core),(0.5,outer_core),(0.95,outer_core),(1.0,crust)]
       material $
           do pigment $ planet_interior (pure blackbody) (pure blackbody) $ cities (pure black) planet_surface
              emissive $ planet_interior (pure yellow) (pure red) $ cities (pure $ scaleRGB 0.2 white) (pure blackbody)
              specular 20 $ planet_interior (pure blackbody) (pure blackbody) $ land_vs_water (pure blackbody) (pure white)

moon :: Modeling ()
moon = model $ 
    do sphere (Point3D 0 0 0) 0.2
       material $ pigment $ pattern (cloudy 8 0.05) [(0.0,pure slate_gray),(1.0,pure white)]
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
    do closedDisc (Point3D 0 (-0.1) 0) (Vector3D 0 1 0) 75
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
	        do pigment $ pure silver
                   specular 100 $ pure silver
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
           do pigment $ pure gold
              specular 64 $ pure silver

glow_orb :: Modeling ()
glow_orb = translate (Vector3D 0 1 0) $
    do closedDisc (Point3D 0 0 0) (Vector3D 0 1 0) 1
       material $ emissive $ pattern (spherical (Point3D 0 0 0) 1) [(0.0,pure $ scaleRGB 1.5 white),(0.25,pure white),(0.95,pure blackbody)]

orb_upper_leg :: Modeling ()
orb_upper_leg = model $
    do tube $ zipCurve (,) (pure 0.05) $ linearInterpolation [Point3D 0 0 0,Point3D 0 0.1 0.5,Point3D 0 0 1]
       sphere (Point3D 0 0 1) 0.05
       material $
           do pigment $ pure gold
              specular 64 $ pure silver

orb_lower_leg :: Modeling ()
orb_lower_leg = model $
    do openCone (Point3D 0 0 0,0.05) (Point3D 0 0 1,0)
       material $ 
           do pigment $ pure gold
              specular 64 $ pure silver

sky :: Modeling ()
sky = model $
    do skyHemisphere 0.6 1.0 (Point3D 0 0 0) (Vector3D 0 1 0) 1.0
       material $
           do emissive $ scaleRGB 0.5 <$> (pattern (gradient origin_point_3d $ Vector3D 0 1 0) 
		  [(0.0,pure white),(0.1,pure azure),(0.4,pure blackbody)])
