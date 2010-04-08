{-# LANGUAGE OverloadedStrings #-}

module Models.Sky
    (SkyInfo(..),default_sky,
     SunInfo(..),default_sun,
     sunInfoOf,
     makeSky,
     makeSun,
     skyAbsorbtionFilter,
     sunVector,
     sunColor,
     LightingConfiguration(..),
     lightingConfiguration,
     ambientSkyRadiation)
    where

import RSAGL.Math
import RSAGL.Extras.Sky
import RSAGL.Extras.ColorPhysics
import RSAGL.Modeling
import RSAGL.Modeling.Noise
import Scene
import Data.Monoid
import RSAGL.Types
import qualified Data.ByteString as B

data SkyInfo = SkyInfo {
    sky_info_biome :: B.ByteString,
    sky_info_degrees_after_midnight :: Integer,
    sky_info_degrees_latitude :: Integer,
    sky_info_degrees_axial_tilt :: Integer,
    -- | indicates current season, where 0 degrees is summer in northern hemisphere (positive latitude)
    -- given positive axial tilt, and 180 degrees is winter.
    sky_info_degrees_orbital :: Integer, 
    sky_info_solar_kelvins :: Integer }
        deriving (Eq,Ord,Show)

data SunInfo = SunInfo {
    -- | on a logarithmic scale base 1.01, i.e. 5 means multiply the size by 1.01**5
    sun_info_size_adjustment :: Integer,
    sun_info_kelvins :: Integer }
        deriving (Eq,Ord,Show)

default_sky :: SkyInfo
default_sky = SkyInfo {
    sky_info_biome = "oceanbiome",
    sky_info_degrees_after_midnight = 0,
    sky_info_degrees_latitude = 100,
    sky_info_degrees_axial_tilt = 0,
    sky_info_degrees_orbital = 90,
    sky_info_solar_kelvins = 5800 }

default_sun :: SunInfo
default_sun = sunInfoOf default_sky

-- | generates a 'SunInfo' from a 'SkyInfo'
sunInfoOf :: SkyInfo -> SunInfo
sunInfoOf sky_info = SunInfo {
    sun_info_size_adjustment = abs (sky_info_degrees_latitude sky_info) + (fst $ biomeAtmosphere $ sky_info_biome sky_info),
    sun_info_kelvins = sky_info_solar_kelvins sky_info }

medium_atmosphere :: Atmosphere
medium_atmosphere = [
    AtmosphereLayer Air   0.75 9.0e-4,
    AtmosphereLayer Vapor 0.25 2.5e-4,
    AtmosphereLayer (Dust $ gray 0.5) 0.01 1.0e-4 ]

thin_atmosphere :: Atmosphere
thin_atmosphere = [
    AtmosphereLayer Air 0.075 9.0e-5,
    AtmosphereLayer Vapor 0.025 2.5e-5 ]

thick_atmosphere :: Atmosphere
thick_atmosphere = [
    AtmosphereLayer Air 1.5 9.0e-4,
    AtmosphereLayer Vapor 1.0 2.5e-4,
    AtmosphereLayer (Dust $ gray 0.5) 0.02 1.0e-4 ]

arid_atmosphere :: Atmosphere
arid_atmosphere = [
    AtmosphereLayer Air 0.05 2.5e-3,
    AtmosphereLayer (Dust $ rust) 0.1 1.0e-3 ]

biomeAtmosphere :: B.ByteString -> (Integer,Atmosphere)
biomeAtmosphere "rockbiome" = (0,arid_atmosphere)
biomeAtmosphere "icyrockbiome" = (-100,thin_atmosphere)
biomeAtmosphere "grasslandbiome" = (35,medium_atmosphere)
biomeAtmosphere "tundrabiome" = (-75,thin_atmosphere)
biomeAtmosphere "desertbiome" = (100,arid_atmosphere)
biomeAtmosphere "oceanbiome" = (5,medium_atmosphere)
biomeAtmosphere "mountainbiome" = (-15,thin_atmosphere)
biomeAtmosphere "swampbiome" = (35,thick_atmosphere)
biomeAtmosphere _ = (0,[])

-- | 'sunVectorOf' indicates vector pointing at the sun.
sunVector :: SkyInfo -> Vector3D
sunVector sky_info = 
    rotate (Vector3D 1 0 0) (fromDegrees $ (realToFrac $ sky_info_degrees_latitude sky_info) +
                                             (cosine $ fromDegrees $ realToFrac $ sky_info_degrees_orbital sky_info) * 
					     (realToFrac $ sky_info_degrees_axial_tilt sky_info)) $
    rotate (Vector3D 0 0 1) (fromDegrees $ realToFrac $ sky_info_degrees_after_midnight sky_info) $ 
    Vector3D 0 (-1) 0

-- | Apparent temperature of a color in kelvins.
temperatureColor :: Integer -> RGB
temperatureColor kelvins = lerpBetweenClamped (770,realToFrac kelvins,1060) 
                                         (gray 0,maximizeRGB $ blackBodyRGB $ realToFrac kelvins)

-- | Apparent color of light comming from the sun.
sunColor :: SunInfo -> RGB
sunColor sun_info = temperatureColor (sun_info_kelvins sun_info)

-- | The size of a very ordinary sun-like star as seen from a very temperate climate.
base_star_size :: RSdouble
base_star_size = 0.1

-- | Radius of the sun at a standard distance eye-to-center of 10 units.
sunSize :: SunInfo -> RSdouble
sunSize sun_info = base_star_size * (5800^2 / (realToFrac $ sun_info_kelvins sun_info ^2)) * 1.01 ** (realToFrac $ sun_info_size_adjustment sun_info)

-- | 'makeSky' generates a sky sphere.
makeSky :: SkyInfo -> Modeling ()
makeSky sky_info = model $
    do hilly_silhouette 
       model $
           do let v = sunVector sky_info
              skyHemisphere origin_point_3d (Vector3D 0 1 0) 5.0
              affine $ scale (Vector3D 2 1 2)
              material $ atmosphereScatteringMaterial (snd $ biomeAtmosphere $ sky_info_biome sky_info)
                                                      [(v,maximizeRGB $ blackBodyRGB $ realToFrac $ sky_info_solar_kelvins sky_info)] 
					              (dynamicSkyFilter 0.05 0.5)

-- | Implements absorbtion of light sources passing through the sky sphere.  In particular, this turns off all lights
-- inside 'scene_layer_sky_sphere'.
skyAbsorbtionFilter :: SkyInfo -> LightSourceLayerTransform
skyAbsorbtionFilter sky_info = LightSourceLayerTransform $ \entering_layer originating_layer ls -> let v = direction ls in
    case () of
        () | entering_layer == scene_layer_sky_sphere || isNoLight ls -> NoLight
	() | originating_layer <= scene_layer_sky_sphere || entering_layer > scene_layer_sky_sphere || originating_layer <= entering_layer -> ls
	() | originating_layer == scene_layer_distant && entering_layer == scene_layer_orbit -> mapLightSource (mapBoth $ scaleRGB $ sunlightFadeFactor (fromDegrees 30) v) ls
	() | entering_layer == scene_layer_far_sky -> sunFade (fromDegrees 10) v ls
	() | entering_layer == scene_layer_clouds -> sunFade (fromDegrees 5) v ls
	() | entering_layer == scene_layer_near_sky -> sunFade (fromDegrees 2) v ls
	() | otherwise -> sunFade (fromDegrees 0) v ls
  where absorbtion v = filterRGB $ (absorbtionFilter . absorbtionFilter) $ atmosphereAbsorbtion (snd $ biomeAtmosphere $ sky_info_biome sky_info) (Point3D 0 1 0) v
	direction (PointLight { lightsource_position = p }) = vectorToFrom p origin_point_3d
	direction (DirectionalLight { lightsource_direction = d }) = d
	direction NoLight = Vector3D 0 1 0
	sunFade tolerance v = mapLightSource (mapBoth (absorbtion v) `mappend` mapBoth (scaleRGB $ sunlightFadeFactor tolerance v))

-- | The amount of fade of the sun based on falling below the horizon.
sunlightFadeFactor :: Angle -> Vector3D -> RSdouble
sunlightFadeFactor tolerance v = max 0 $ lerpBetweenClamped (85,toDegrees $ angleBetween v (Vector3D 0 1 0),95+toDegrees tolerance) (1.0,0.0)

-- | Information about the lighting environment.  All values are between 0 and 1, indicating a relative scale compared to the normal, full brightness.
data LightingConfiguration = LightingConfiguration {
    -- | Apparent brightness of the sun.  This will fade to zero along the horizon and equal zero at night.
    lighting_sunlight, 
    -- | Apparent brightness of ambient sky radiation.  This will fade to black more slowly than 'lighting_sunlight', lingering after the sun has set.
    lighting_skylight, 
    -- | Apparent brightness of the nightlight.  This is a blue light with heavy ambient component that simulates human night vision.
    lighting_nightlight, 
    -- | Brightness of artificial lights.  Typically all artificial lights intended for nighttime illumination should be scaled based on this value.
    lighting_artificial :: RSdouble }

lightingConfiguration :: SkyInfo -> LightingConfiguration
lightingConfiguration sky_info = result
    where result = LightingConfiguration {
                       lighting_sunlight = sunlightFadeFactor (fromDegrees 0) (sunVector sky_info),
		       lighting_skylight = sunlightFadeFactor (fromDegrees 10) (sunVector sky_info),
		       lighting_nightlight = max 0 $ 1.0 - lighting_sunlight result - lighting_skylight result,
		       lighting_artificial = min 1 $ max 0 $ 1.0 - lighting_sunlight result - (lighting_nightlight result/4) + (1000 / realToFrac (sky_info_solar_kelvins sky_info))^3 }

-- | Get the color of the ambient sky radiation by sampling a very small number vectors into the sky.
ambientSkyRadiation :: SkyInfo -> RGB
ambientSkyRadiation sky_info = abstractAverage $ map (atmosphereScattering atmosphere [sun_info] (Point3D 0 1 0)) test_vectors
    where atmosphere = snd $ biomeAtmosphere $ sky_info_biome sky_info
          sun_info = (sunVector sky_info,sunColor $ sunInfoOf sky_info)
	  test_vectors = map vectorNormalize $ 
	      do x <- [1,0,-1]
	         y <- [1,0,-1]
		 return $ Vector3D x 1 y

-- 'makeSun' generates a perspectiveSphere of the sun.
makeSun :: SunInfo -> Modeling ()
makeSun sun_info = model $
    do let size = sunSize sun_info
       let temp = sun_info_kelvins sun_info
       let temperaturePattern t = pattern (cloudy (fromInteger $ temp + sun_info_size_adjustment sun_info) base_star_size) 
               [(0.0,pure $ temperatureColor $ t + 700),(0.5,pure $ temperatureColor t),(1.0,pure $ temperatureColor $ t - 700)]
       perspectiveSphere (Point3D 0 (-10) 0) size origin_point_3d
       material $ 
           do pigment $ pure $ gray 0
	      emissive $ pattern (spherical (Point3D 0 (size-10) 0) size) [(0.0,temperaturePattern temp),
                                                                           (0.5,temperaturePattern $ temp - 200),
                                                                           (0.75,temperaturePattern $ temp - 500),
                                                                           (0.9,temperaturePattern $ temp - 800),
                                                                           (1.0,temperaturePattern $ temp - 1000)]

hilly_silhouette :: Modeling ()
hilly_silhouette = model $
    do heightDisc (0,0) 8 (\(x,z) -> perlinNoise (Point3D x 0 z) - 6.9 + distanceBetween origin_point_3d (Point3D x 0 z))
       affine $ scale (Vector3D 1 0.2 1)
       material $ pigment $ pure blackbody
       disregardSurfaceNormals

