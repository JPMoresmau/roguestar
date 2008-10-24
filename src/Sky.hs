{-# LANGUAGE Arrows #-}

module Sky
    (getSkyInfo,
     sky,
     Models.Sky.default_sky,
     Models.Sky.default_sun,
     skyAbsorbtionFilter,
     SkyInfo)
    where

import Models.Sky
import Animation
import Scene
import Models.LibraryData
import RSAGL.Affine
import RSAGL.CoordinateSystems
import RSAGL.Vector
import RSAGL.Edge
import Data.Maybe
import Control.Arrow
import RSAGL.LightSource
import RSAGL.RSAGLColors

getSkyInfo :: RSAnimAX k t i o () SkyInfo
getSkyInfo = proc () ->
    do m_biome <- sticky isJust Nothing <<< driverGetAnswerA -< "biome"
       returnA -< default_sky { sky_info_biome = fromMaybe "" m_biome }

sky :: RSAnimAX k t i o SkyInfo ()
sky = proc sky_info ->
    do libraryA -< (scene_layer_sky_sphere,SkySphere sky_info)
       transformA sun -< (affineOf $ rotateToFrom (sunVector sky_info) (Vector3D 0 (-1) 0),sky_info)

sun :: RSAnimAX k t i o SkyInfo ()
sun = proc sky_info ->
    do libraryA -< (scene_layer_distant,SunDisc $ sunInfoOf sky_info)
       accumulateSceneA -< (scene_layer_distant,lightSource $ PointLight {
           lightsource_position = Point3D 0 (-10) 0,
	   lightsource_radius = measure origin_point_3d (Point3D 0 (-10) 0),
	   lightsource_color = sunColor $ sunInfoOf sky_info,
	   lightsource_ambient = blackbody})

