module RSAGL.Scene.LightSource
    (LightSource(..),
     skylight,
     MapLightSource,
     mapLight,
     mapAmbient,
     mapBoth,
     mapLightSource,
     isNoLight,
     infiniteLightSourceOf,
     setLightSourcesToOpenGL,
     setLightSourceToOpenGL)
    where

import RSAGL.Math.Vector
import RSAGL.Modeling.Color as Color
import RSAGL.Math.Affine
import RSAGL.Scene.CoordinateSystems
import Graphics.UI.GLUT as GLUT
import Data.List as List
import Data.Monoid

-- | A light source.  In addition to position information, each type of 
-- light source (except 'NoLight') has a "color" term, indicating the color of direct lighting,
-- and an "ambient" term, indicating the color of indirect lighting.
data LightSource =
    -- | A directional or infinite light source, which always appears to be comming from the specified vector
      DirectionalLight { lightsource_direction :: Vector3D,
                         lightsource_color :: Color.RGB,
                         lightsource_ambient :: Color.RGB }
    -- | A point light, which has a specific position.  The intensity of point light radiation falls off with the
    -- inverse square of distance, normalized to one at the specified radius.  This means that point lights will
    -- be overbright inside that radius.
    | PointLight { lightsource_position :: Point3D,
                   lightsource_radius :: Distance,
                   lightsource_color :: Color.RGB,
                   lightsource_ambient :: Color.RGB }
    -- | An inactive light source, equivalent to any light source with a color and ambient term of "rgb 0 0 0".
    | NoLight

-- | A DirectionalLight approximation of ambient sky radiation.  Normally, set the vector to straight up, since that is the direction of the sky.
skylight :: Vector3D -> Color.RGB -> LightSource
skylight v c = DirectionalLight {
    lightsource_direction = v,
    lightsource_color = scaleRGB 0.7208681020859709 c,
    lightsource_ambient = scaleRGB 0.27913189791402915 c }

-- | 'True' if a light is 'NoLight'.
isNoLight :: LightSource -> Bool
isNoLight NoLight = True
isNoLight _ = False

-- | Encodes a transformation of a light source, including the light color and ambient color terms, and any affine transformation.
-- The 'Monoid' instance supports 'MapLightSource' as a specialized endomorphism (see 'Endo').
data MapLightSource = MapLightSource { map_light, map_ambient :: (RGB -> RGB), map_affine :: AffineTransformation }

instance Monoid MapLightSource where
    mempty = MapLightSource { map_light = id, map_ambient = id, map_affine = id }
    mappend x y = MapLightSource {
        map_light = map_light x . map_light y,
	map_ambient = map_ambient x . map_ambient y,
	map_affine = map_affine x . map_affine y }

instance AffineTransformable MapLightSource where
    transform m f = f { map_affine = transform m . map_affine f }

-- | Transformation of the direct illumination color of a light source.
mapLight :: (RGB -> RGB) -> MapLightSource
mapLight f = mempty { map_light = f }

-- | Transformation of the ambient color of a light source.
mapAmbient :: (RGB -> RGB) -> MapLightSource
mapAmbient f = mempty { map_ambient = f }

-- | Transformation of both the direct illumination and the ambient color terms of a light source.
mapBoth :: (RGB -> RGB) -> MapLightSource
mapBoth f = mapLight f `mappend` mapAmbient f

-- | Apply a 'MapLightSource' to a 'LightSource'.
mapLightSource :: MapLightSource  -> LightSource -> LightSource
mapLightSource f (DirectionalLight source_direction source_color source_ambient) = 
    transformation (map_affine f) $ DirectionalLight source_direction (map_light f source_color) (map_ambient f source_ambient)
mapLightSource f (PointLight source_position source_radius source_color source_ambient) = 
    transformation (map_affine f) $ PointLight source_position source_radius (map_light f source_color) (map_ambient f source_ambient)
mapLightSource _ NoLight = NoLight

-- | Converts a 'PointLight' to a 'DirectionalLight', assuming that the camera is at the origin.
infiniteLightSourceOf :: LightSource -> LightSource
infiniteLightSourceOf NoLight = NoLight
infiniteLightSourceOf (d@(DirectionalLight {})) = d
infiniteLightSourceOf (p@PointLight {}) = DirectionalLight {
    lightsource_direction = vectorToFrom (lightsource_position p) origin_point_3d,
    lightsource_color = scaleRGB scale_factor $ lightsource_color p,
    lightsource_ambient = scaleRGB scale_factor $ lightsource_ambient p }
        where scale_factor = realToFrac $ (distanceSquared $ lightsource_radius p) / (distanceBetweenSquared origin_point_3d (lightsource_position p))

instance AffineTransformable LightSource where
    transform _ NoLight = NoLight
    transform m (dl@(DirectionalLight {})) = dl { lightsource_direction = transform m $ lightsource_direction dl }
    transform m (pl@(PointLight {})) = pl {
        lightsource_position = transform m $ lightsource_position pl,
        lightsource_radius = transform m $ lightsource_radius pl }

-- | Set OpenGL light sources starting from 0.
setLightSourcesToOpenGL :: [LightSource] -> IO ()
setLightSourcesToOpenGL lss =
    do max_lights <- GLUT.get maxLights
       mapM_ setLightSourceToOpenGL $ genericTake max_lights $ zip (map Light [0..]) (lss ++ repeat NoLight)

-- | Set one specific OpenGL light source.
setLightSourceToOpenGL :: (Light,LightSource) -> IO ()
setLightSourceToOpenGL (l,NoLight) = light l $= Disabled
setLightSourceToOpenGL (l,dl@DirectionalLight { lightsource_color = Color.RGB cr cg cb,
                                        lightsource_ambient = Color.RGB ar ag ab }) =
    do let Vector3D vx vy vz = vectorNormalize $ lightsource_direction dl
       light l $= Enabled
       ambient l $= (Color4 (realToFrac ar) (realToFrac ag) (realToFrac ab) 1.0 :: Color4 Float)
       GLUT.specular l $= (Color4 (realToFrac cr) (realToFrac cg) (realToFrac cb) 1.0 :: Color4 Float)
       diffuse l $= (Color4 (realToFrac cr) (realToFrac cg) (realToFrac cb) 1.0 :: Color4 Float)
       position l $= (Vertex4 (realToFrac vx) (realToFrac vy) (realToFrac vz) 0 :: Vertex4 Float)
       attenuation l $= (1,0,0)
setLightSourceToOpenGL (l,pl@(PointLight { lightsource_position = (Point3D px py pz),
                                   lightsource_color = Color.RGB cr cg cb,
                                   lightsource_ambient = Color.RGB ar ag ab })) =
    do light l $= Enabled
       ambient l $= (Color4 (realToFrac ar) (realToFrac ag) (realToFrac ab) 1.0 :: Color4 Float)
       GLUT.specular l $= (Color4 (realToFrac cr) (realToFrac cg) (realToFrac cb) 1.0 :: Color4 Float)
       diffuse l $= (Color4 (realToFrac cr) (realToFrac cg) (realToFrac cb) 1.0 :: Color4 Float)
       position l $= (Vertex4 (realToFrac px) (realToFrac py) (realToFrac pz) 1 :: Vertex4 Float)
       attenuation l $= (0.01,0,recip $ realToFrac $ distanceSquared $ lightsource_radius pl)
