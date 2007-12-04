module Models.PlanetRingMoon
    (planet_ring_moon)
    where

import RSAGL.Model
import RSAGL.Vector
import Data.Monoid
import RSAGL.ModelingExtras

planet_ring_moon :: (Monoid attr) => Modeling attr
planet_ring_moon = model $
    do model $ do disc 0.75 1.0
                  transparent $ pure $ alpha 0.25 purple
                  specular 2 $ pure purple
                  waves 0.5 0.02
       model $ do sphere (Point3D 0 0 0) 0.65
                  let land_vs_water land water = cloudy 1.0 [(0,water),(0.45,water),(0.55,land),(1,land)]
                  let grass_and_mountains = cloudy 0.25 [(0.2,pure slate_gray),(0.3,pure forest_green)]
                  let land_and_water = land_vs_water grass_and_mountains (pure blue)
                  let cities bright dark = land_vs_water (cloudy 0.1 [(0.0,bright),(0.3,dark)]) (dark)
                  let planet_surface = gradient (Point3D 0 0 0) (Vector3D 0 0.65 0) [(-0.9,pure white),(-0.85,land_and_water),(0.85,land_and_water),(0.9,pure white)]
                  pigment $ cities (pure black) planet_surface
                  emissive $ cities (pure $ scaleRGB 0.2 yellow) (pure blackbody)
                  specular 20 $ land_vs_water (pure blackbody) (pure white)
       model $ do sphere (Point3D 0 0 4) 0.2
                  pigment $ cloudy 0.05 [(0.0,pure silver),(1.0,pure black)]