{-# LANGUAGE Arrows #-}

module Sky
    (getSkyInfo,
     sky,
     Models.Sky.default_sky,
     Models.Sky.default_sun,
     skyAbsorbtionFilter,
     SkyInfo,
     Models.Sky.LightingConfiguration(..),
     Sky.lightingConfiguration)
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
import RSAGL.InverseKinematics
import RSAGL.Time
import RSAGL.Color
import RSAGL.FRP
import RSAGL.Angle

-- | Get the current SkyInfo data for the current planet.
-- REVISIT: this randomly generates solar information, eventually it needs to be generated in the engine
getSkyInfo :: RSAnimAX k t i o () SkyInfo
getSkyInfo = proc () ->
    do t1 <- randomA -< (10000,2000)
       t2 <- randomA -< (10000,2000)
       t3 <- randomA -< (10000,2000)
       t4 <- randomA -< (10000,2000)
       temperature <- initial -< minimum [t1,t2,t3,t4] -- low temeratures are more probable
       m_biome <- sticky isJust Nothing <<< driverGetAnswerA -< "biome"
       returnA -< default_sky { sky_info_biome = fromMaybe "" m_biome,
                                sky_info_solar_kelvins = temperature }

sky :: RSAnimAX k t i o SkyInfo ()
sky = proc sky_info ->
    do libraryA -< (scene_layer_sky_sphere,SkySphere sky_info)
       let sun_vector = sunVector sky_info
       whenJust (transformA sun) -< if angleBetween sun_vector (Vector3D 0 1 0) < fromDegrees 135
           then Just (affineOf $ rotateToFrom (sunVector sky_info) (Vector3D 0 (-1) 0),sky_info)
	   else Nothing
       lighting_configuration <- Sky.lightingConfiguration -< sky_info
       let nightlight_intensity = lighting_nightlight lighting_configuration
       let skylight_intensity = lighting_skylight lighting_configuration
       skylight_color <- edgeMap ambientSkyRadiation -< sky_info
       accumulateSceneA -< (scene_layer_local,lightSource $ if nightlight_intensity > 0.05
           then mapLightSource (mapBoth $ scaleRGB $ nightlight_intensity) $ DirectionalLight {
                    lightsource_direction = Vector3D 0 1 0,
	            lightsource_color = rgb 0.1 0.1 0.2,
	            lightsource_ambient = rgb 0.0 0.0 0.3 }
           else NoLight)
       accumulateSceneA -< (scene_layer_local,lightSource $ if skylight_intensity > 0.05
           then mapLightSource (mapBoth $ scaleRGB $ lighting_skylight lighting_configuration) $ skylight (Vector3D 0 1 0) skylight_color
	   else NoLight)

sun :: RSAnimAX k t i o SkyInfo ()
sun = proc sky_info ->
    do libraryA -< (scene_layer_distant,SunDisc $ sunInfoOf sky_info)
       accumulateSceneA -< (scene_layer_distant,lightSource $ PointLight {
           lightsource_position = Point3D 0 (-10) 0,
	   lightsource_radius = measure origin_point_3d (Point3D 0 (-10) 0),
	   lightsource_color = sunColor $ sunInfoOf sky_info,
	   lightsource_ambient = blackbody})

lightingConfiguration :: RSAnimAX k t i o SkyInfo LightingConfiguration
lightingConfiguration = proc sky_info ->
    do nightlight <- approachA 1.0 (perSecond 1.0) -< lighting_nightlight $ Models.Sky.lightingConfiguration sky_info
       artificial <- approachA 1.0 (perSecond 1.0) -< lighting_artificial $ Models.Sky.lightingConfiguration sky_info
       returnA -< (Models.Sky.lightingConfiguration sky_info) {
           lighting_nightlight = nightlight,
	   lighting_artificial = artificial }
