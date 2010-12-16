{-# LANGUAGE Arrows, OverloadedStrings, TypeFamilies #-}

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
import Tables
import Scene
import Models.LibraryData
import RSAGL.Math
import RSAGL.FRP
import Data.Maybe
import Control.Arrow
import RSAGL.Color
import RSAGL.Color.RSAGLColors
import RSAGL.Animation
import System.Random ()
import Control.Monad.Random
import Globals
import qualified Data.ByteString.Char8 as B

-- | Get the current SkyInfo data for the current planet.
getSkyInfo :: (FRPModel m,StateOf m ~ AnimationState,
               InputOutputOf m ~ Enabled) => FRP e m () SkyInfo
getSkyInfo = proc () ->
    do random_id <- sticky isJust Nothing <<< driverGetAnswerA -< "plane-random-id"
       m_biome <- sticky isJust Nothing <<< driverGetAnswerA -< "biome"
       clingy Discrete (==) (uncurry generateSkyInfo) -< (random_id,m_biome)

generateSkyInfo :: Maybe B.ByteString -> Maybe B.ByteString -> SkyInfo
generateSkyInfo random_id m_biome = fst $ flip runRand (mkStdGen $ fromInteger $ fromMaybe 0 $ random_id >>= readInteger) $
     do temperature <- getRandomR (10000,2000)
        degrees_after_midnight <- getRandomR (0,360)
        degrees_latitude <- getRandomR(-90,90)
        degrees_axial_tilt <- getRandomR (0,90)
        degrees_orbital <- getRandomR (0,360)
        return $ default_sky { sky_info_biome = fromMaybe "nothing" m_biome,
                               sky_info_solar_kelvins = temperature,
                               sky_info_degrees_after_midnight = degrees_after_midnight,
                               sky_info_degrees_latitude = degrees_latitude,
                               sky_info_degrees_axial_tilt = degrees_axial_tilt,
                               sky_info_degrees_orbital = degrees_orbital }

sky :: (FRPModel m,StateOf m ~ AnimationState,
        InputOutputOf m ~ Enabled) => FRP e m SkyInfo ()
sky = proc sky_info ->
    do sky_on <- readGlobal global_sky_on -< ()
       libraryA -< (scene_layer_sky_sphere,if sky_on then SkySphere sky_info else NullModel)
       let sun_vector = sunVector sky_info
       whenJust (transformA sun) -< if angleBetween sun_vector (Vector3D 0 1 0) < fromDegrees 135 && sky_on
           then Just (affineOf $ rotateToFrom (sunVector sky_info) (Vector3D 0 (-1) 0),sky_info)
           else Nothing
       returnA -< ()
       lighting_configuration <- Sky.lightingConfiguration -< sky_info
       let nightlight_intensity = lighting_nightlight lighting_configuration
       let skylight_intensity = lighting_skylight lighting_configuration
       skylight_color <- clingy Discrete (==) ambientSkyRadiation -< sky_info
       accumulateSceneA -< (scene_layer_local,lightSource $ case () of
           () | nightlight_intensity > 0.05 && sky_on ->
                    mapLightSource (mapBoth $ scalarMultiply nightlight_intensity) $
                        DirectionalLight {
                            lightsource_direction = Vector3D 0 1 0,
                            lightsource_color = rgb 0.1 0.1 0.2,
                            lightsource_ambient = rgb 0.0 0.0 0.3 }
           () | otherwise -> NoLight)
       accumulateSceneA -< (scene_layer_local,lightSource $ case () of
           () | skylight_intensity > 0.05 && sky_on ->
                    mapLightSource (mapBoth $ scalarMultiply skylight_intensity) $
                        skylight (Vector3D 0 1 0) skylight_color
           () | lighting_artificial lighting_configuration <= 0.05 &&
                not sky_on ->
                        skylight (Vector3D 0 1 0) white
           () | otherwise -> NoLight)

sun :: (FRPModel m,StateOf m ~ AnimationState,
        InputOutputOf m ~ Enabled) => FRP e m SkyInfo ()
sun = proc sky_info ->
    do libraryA -< (scene_layer_distant,maybe NullModel SunDisc $ sunInfoOf sky_info)
       lighting_configuration <- Sky.lightingConfiguration -< sky_info
       sky_on <- readGlobal global_sky_on -< ()
       let sunlight_intensity = lighting_sunlight lighting_configuration
       accumulateSceneA -< (scene_layer_distant,lightSource $ case sunInfoOf sky_info of
            Just sun_info | sunlight_intensity > 0.05 && sky_on ->
                     PointLight {
                         lightsource_position = Point3D 0 (-10) 0,
                         lightsource_radius = measure origin_point_3d
                                                      (Point3D 0 (-10) 0),
                         lightsource_color = sunColor sun_info,
                         lightsource_ambient = blackbody}
            _ | otherwise -> NoLight)

lightingConfiguration :: (FRPModel m) => FRP e m SkyInfo LightingConfiguration
lightingConfiguration = proc sky_info ->
    do nightlight <- approachA 1.0 (perSecond 1.0) -< lighting_nightlight $ Models.Sky.lightingConfiguration sky_info
       artificial <- approachA 1.0 (perSecond 1.0) -< lighting_artificial $ Models.Sky.lightingConfiguration sky_info
       returnA -< (Models.Sky.lightingConfiguration sky_info) {
           lighting_nightlight = nightlight,
           lighting_artificial = artificial }

