\section{Light Sources}

\begin{code}
module RSAGL.LightSource
    (LightSource(..),
     skylight,
     isNoLight,
     infiniteLightSourceOf,
     setLightSourcesToOpenGL,
     setLightSourceToOpenGL)
    where

import RSAGL.Vector
import RSAGL.Color as Color
import RSAGL.Affine
import RSAGL.CoordinateSystems
import Graphics.UI.GLUT as GLUT
import Data.List as List
\end{code}

\begin{code}
data LightSource =
      DirectionalLight { lightsource_direction :: Vector3D,
                         lightsource_color :: Color.RGB,
                         lightsource_ambient :: Color.RGB }
    | PointLight { lightsource_position :: Point3D,
                   lightsource_radius :: Distance,
                   lightsource_color :: Color.RGB,
                   lightsource_ambient :: Color.RGB }
    | NoLight

skylight :: Vector3D -> Color.RGB -> LightSource
skylight v c = DirectionalLight {
    lightsource_direction = v,
    lightsource_color = scaleRGB 0.7208681020859709 c,
    lightsource_ambient = scaleRGB 0.27913189791402915 c }

isNoLight :: LightSource -> Bool
isNoLight NoLight = True
isNoLight _ = False

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

setLightSourcesToOpenGL :: [LightSource] -> IO ()
setLightSourcesToOpenGL lss =
    do max_lights <- GLUT.get maxLights
       mapM_ setLightSourceToOpenGL $ genericTake max_lights $ zip (map Light [0..]) (lss ++ repeat NoLight)

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
\end{code}
