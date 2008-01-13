module Models.PlanetRingMoon
    (planet,ring,moon,ground,monolith)
    where

import RSAGL.Model
import RSAGL.Vector
import RSAGL.ModelingExtras
import RSAGL.RayTrace
import RSAGL.Deformation
import RSAGL.Affine

ring :: Modeling ()
ring = model $ do disc 0.75 1.0
                  transparent $ pure $ alpha 0.25 purple
                  specular 2 $ pure purple
                  bumps $ waves 0.2 0.01

planet :: Modeling ()
planet=model $ do sphere (Point3D 0 0 0) 0.65
                  deform $ constrain (\(SurfaceVertex3D (Point3D x y z) _) -> x > 0 && y > 0 && z > 0) $ 
                               shadowDeform (Vector3D (-1) (-1) (-1)) (map (plane (Point3D 0 0 0)) [Vector3D 1 0 0,Vector3D 0 1 0,Vector3D 0 0 1])
                  let land_vs_water land water = pattern (cloudy 26 0.4) [(0,water),(0.5,water),(0.51,land),(1,land)]
                  let grass_and_mountains = pattern (cloudy 81 0.25) [(0.4,pattern (cloudy 99 0.1) [(0.0,pure brown),(1.0,pure slate_gray)]),(0.5,pure forest_green)]
                  let land_and_water = land_vs_water grass_and_mountains (pure blue)
                  let cities bright dark = land_vs_water (pattern (cloudy 5 0.1) [(0.0,bright),(0.5,dark)]) (dark)
                  let planet_surface = pattern (gradient (Point3D 0 0 0) (Vector3D 0 0.65 0)) 
                                           [(-0.9,pure white),(-0.85,land_and_water),(0.85,land_and_water),(0.9,pure white)]
                  let planet_interior inner_core outer_core crust = pattern (spherical (Point3D 0 0 0) 0.65) 
                          [(0.0,inner_core),(0.25,inner_core),(0.5,outer_core),(0.95,outer_core),(1.0,crust)]
                  pigment $ planet_interior (pure blackbody) (pure blackbody) $ cities (pure black) planet_surface
                  emissive $ planet_interior (pure yellow) (pure red) $ cities (pure $ scaleRGB 0.2 white) (pure blackbody)
                  specular 20 $ planet_interior (pure blackbody) (pure blackbody) $ land_vs_water (pure blackbody) (pure white)

moon :: Modeling ()
moon = model $ 
    do sphere (Point3D 0 0 0) 0.2
       pigment $ pattern (cloudy 8 0.05) [(0.0,pure silver),(1.0,pure black)]

monolith :: Modeling ()
monolith = model $
    do smoothbox 0.1 (Point3D 4 9 1) (Point3D (-4) (-9) (-1))
       affine (translate $ Vector3D 0 9 0)
       affine (scale' 0.20)
       pigment $ pure blackbody
       specular 100 $ pure white

ground :: Modeling ()
ground = model $
    do finitePlane 50 (Point3D 0 (-0.1) 0) (Vector3D 0 1 0)
       pigment $ pattern (cloudy 27 1.0) [(0.0,pure brown),(1.0,pure forest_green)]