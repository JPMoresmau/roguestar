module Models.Sky
    (SkyInfo(..),default_sky,
     SunInfo(..),default_sun,
     sunInfoOf,
     makeSky,
     makeSun,
     sunVector,
     sunColor)
    where

import RSAGL.Vector
import RSAGL.Affine
import RSAGL.Extras.Sky
import RSAGL.Angle
import RSAGL.Extras.ColorPhysics
import RSAGL.Model
import RSAGL.Interpolation
import RSAGL.ModelingExtras
import RSAGL.Noise

data SkyInfo = SkyInfo {
    sky_info_biome :: String,
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
    sky_info_degrees_latitude = 91,
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

biomeAtmosphere :: String -> (Integer,Atmosphere)
biomeAtmosphere "rockbiome" = (0,arid_atmosphere)
biomeAtmosphere "icyrockbiome" = (-100,thin_atmosphere)
biomeAtmosphere "grasslandbiome" = (20,medium_atmosphere)
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

sunColor :: SunInfo -> RGB
sunColor sun_info = lerpBetweenClamped (770,realToFrac $ sun_info_kelvins sun_info,1060) 
                                         (gray 0,maximizeRGB $ blackBodyRGB $ realToFrac $ sun_info_kelvins sun_info)

-- | Radius of the sun at a standard distance eye-to-center of 10 units.
sunSize :: SunInfo -> Double
sunSize sun_info = 0.05 * (5800 / (realToFrac $ sun_info_kelvins sun_info)) * 1.01 ** (realToFrac $ sun_info_size_adjustment sun_info)

-- | 'makeSky' generates a sky sphere.
makeSky :: SkyInfo -> Modeling ()
makeSky sky_info = model $
    do hilly_silhouette 
       model $
           do let v = sunVector sky_info
              skyHemisphere origin_point_3d (Vector3D 0 1 0) 9.0
              material $ atmosphereScatteringMaterial (snd $ biomeAtmosphere $ sky_info_biome sky_info)
                                                      [(v,maximizeRGB $ blackBodyRGB $ realToFrac $ sky_info_solar_kelvins sky_info)] 
					              (dynamicSkyFilter 0.05 0.5)

-- 'makeSun' generates a perspectiveSphere of the sun.
makeSun :: SunInfo -> Modeling ()
makeSun sun_info = model $
    do perspectiveSphere (Point3D 0 (-10) 0) (sunSize sun_info) origin_point_3d
       material $ 
           do pigment $ pure $ gray 0
	      emissive $ pure $ sunColor sun_info

hilly_silhouette :: Modeling ()
hilly_silhouette = model $
    do heightDisc (0,0) 8 (\(x,z) -> perlinNoise (Point3D x 0 z) - 6.9 + distanceBetween origin_point_3d (Point3D x 0 z))
       material $ pigment $ pure blackbody
       disregardSurfaceNormals

