\section{Terrain Tiles}

\begin{code}
module Models.Terrain
    (known_terrain_types,
     terrainTile)
    where

import Quality
import Models.RecreantFactory
import RSAGL.Model
import RSAGL.ModelingExtras
import RSAGL.Vector
import RSAGL.Curve
import RSAGL.Interpolation
import RSAGL.Affine
import RSAGL.Angle

known_terrain_types :: [String]
known_terrain_types =
    ["water",
     "deepwater",
     "sand",
     "desert",
     "grass",
     "forest",
     "deepforest",
     "ice",
     "lava",
     "glass",
     "rockyground",
     "rubble",
     "rockface",
     "recreantfactory"]

terrainTileShape :: Double -> Double -> Modeling ()
terrainTileShape squash height = model $
    do regularPrism (origin_point_3d,sqrt 0.5) (Point3D 0 1 0,0.0001) 4
       deform $ \(Point3D x y z) -> Point3D x (sqrt $ max 0 y) z
       affine $ scale (Vector3D 1 height 1) . rotate (Vector3D 0 1 0) (fromDegrees 45)
       deform $ \(SurfaceVertex3D p v) -> SurfaceVertex3D (scale (Vector3D 1 squash 1) p) v

terrainTile :: String -> Quality -> Modeling ()
terrainTile "recreantfactory" q = recreant_factory q
terrainTile "rockface" _ = model $
    do terrainTileShape 1.0 (terrainHeight "rockface")
       terrainTexture "rockface"
terrainTile s _ = model $
    do terrainTileShape 0.01 (terrainHeight s)
       terrainTexture s
       
terrainHeight :: String -> Double
terrainHeight "water" = 0.025
terrainHeight "deepwater" = 0.025
terrainHeight "sand" = 0.1
terrainHeight "desert" = 0.1
terrainHeight "grass" = 0.13
terrainHeight "dirt" = 0.12
terrainHeight "forest" = 0.15
terrainHeight "deepforest" = 0.17
terrainHeight "ice" = 0.05
terrainHeight "lava" = 0.06
terrainHeight "glass" = 0.04
terrainHeight "rockyground" = 0.18
terrainHeight "rubble" = 0.2
terrainHeight "rockface" = 0.5
terrainHeight _ = 2.0

terrainTexture :: String -> Modeling ()
terrainTexture "water" =
    do pigment $ pure blue
       specular 100 $ pure white
terrainTexture "deepwater" =
    do pigment $ pure $ scaleRGB 0.8 blue
       specular 100 $ pure white
terrainTexture "sand" = pigment $ pure corn
terrainTexture "desert" = pigment $ pure $ lerp 0.5 (corn,white)
terrainTexture "grass" = pigment $ pure forest_green
terrainTexture "dirt" = pigment $ pure brown
terrainTexture "forest" = pigment $ pure fern_green
terrainTexture "deepforest" = pigment $ pure fern_green
terrainTexture "ice" = 
    do pigment $ pure white
       specular 1 $ pure teal
terrainTexture "lava" =
    do pigment $ pure blackbody
       emissive $ pure coral
terrainTexture "glass" =
    do pigment $ pure black
       specular 1 $ pure white
terrainTexture "rockyground" = pigment $ pure slate_gray
terrainTexture "rubble" = pigment $ pure slate_gray
terrainTexture "rockface" = pigment $ pure slate_gray
terrainTexture _ =
    do pigment $ pure blackbody
       emissive $ pure magenta
\end{code}
