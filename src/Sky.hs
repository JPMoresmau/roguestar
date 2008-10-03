{-# LANGUAGE Arrows #-}

module Sky
    (sky,
     Models.Sky.default_sky,
     Models.Sky.default_sun)
    where

import Models.Sky
import Animation
import RSAGL.Scene
import Models.LibraryData
import RSAGL.Affine
import RSAGL.CoordinateSystems
import RSAGL.Vector

sky :: RSAnimAX k t i o SkyInfo ()
sky = proc sky_info ->
    do libraryA -< (std_scene_layer_infinite+1,SkySphere sky_info)
       transformA libraryA -< (affineOf $ rotateToFrom (sunVector sky_info) (Vector3D 0 (-1) 0),
                               (std_scene_layer_infinite+3,SunDisc $ sunInfoOf sky_info))
