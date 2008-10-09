{-# LANGUAGE Arrows #-}

module Sky
    (getSkyInfo,
     sky,
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
import RSAGL.Edge
import Data.Maybe
import Control.Arrow

getSkyInfo :: RSAnimAX k t i o () SkyInfo
getSkyInfo = proc () ->
    do m_biome <- sticky isJust Nothing <<< driverGetAnswerA -< "biome"
       returnA -< default_sky { sky_info_biome = fromMaybe "" m_biome }

sky :: RSAnimAX k t i o SkyInfo ()
sky = proc sky_info ->
    do libraryA -< (std_scene_layer_infinite+1,SkySphere sky_info)
       transformA libraryA -< (affineOf $ rotateToFrom (sunVector sky_info) (Vector3D 0 (-1) 0),
                               (std_scene_layer_infinite+3,SunDisc $ sunInfoOf sky_info))
