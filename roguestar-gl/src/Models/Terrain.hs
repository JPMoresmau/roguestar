{-# LANGUAGE OverloadedStrings #-}

module Models.Terrain
    (known_terrain_types,
     terrainTile)
    where

import Prelude hiding (tan)
import Quality
import Models.RecreantFactory
import RSAGL.Modeling
import RSAGL.Color.RSAGLColors
import RSAGL.Math
import RSAGL.Types
import qualified Data.ByteString.Char8 as B

-- |
-- A list of all terrain type names known to roguestar-gl.
-- But at any given moment the engine might have been extended to include other types.
known_terrain_types :: [B.ByteString]
known_terrain_types =
    ["water",
     "deepwater",
     "sand",
     "desert",
     "grass",
     "dirt",
     "forest",
     "deepforest",
     "ice",
     "lava",
     "glass",
     "rockyground",
     "rubble",
     "rockface",
     "recreantfactory",
     "downstairs",
     "upstairs"]

-- |
-- A simple 1-by-1 square patch, centered at the origin, and raised on a slope toward it's center.
-- The first parameter indicates the patch's actual height, while the second represent's the patch's
-- apparent height based on it's normal vectors.
-- The sloping effect makes it clear where one tile ends and another begins, which is practical for game play.
-- The two heights are different as it is not desireable that creature's legs should dissappear inside the landscape,
-- but at shallow angles of attack this makes the terrain tile look fake.  REVISIT
--
-- This is just the shape, without any material.
--
terrainTileShape :: RSdouble -> RSdouble -> Quality -> Modeling ()
terrainTileShape physical_height aesthetic_height q = model $
    do heightField (-0.5,-0.5) (0.5,0.5) $ \(x,z) -> let y = 1 - max (abs x) (abs z) * 2 in min (max 0 $ sqrt y) (2*y)
       affine $ scale (Vector3D 1 aesthetic_height 1)
       deform $ \(SurfaceVertex3D p v) -> SurfaceVertex3D (scale (Vector3D 1 (physical_height/aesthetic_height) 1) p) v
       qualityToFixed q

-- |
-- Creates a terrain tile based on 'terrainTileShape' with appropriate characteristics and material for its type,
-- but without any special casing for unsual terrains like forest.
--
basicTerrainTile :: B.ByteString -> Quality -> Modeling ()
basicTerrainTile s q = model $
    do terrainTileShape 0.01 (terrainHeight s) q
       material $ terrainTexture s

-- |
-- Creates a terrain tile based on 'terrainTileShape'
-- Provides special casing for forest, rockface, liquids, etc.
--
terrainTile :: B.ByteString -> Quality -> Modeling ()
terrainTile "recreantfactory" q = recreant_factory q
terrainTile "rockface" q = model $
    do terrainTileShape (terrainHeight "rockface") (terrainHeight "rockface") q
       material $ terrainTexture "rockface"
terrainTile "downstairs" q = model $
    do basicTerrainTile "downstairs" q
       model $
           do box (Point3D (-0.5) 0 (-0.5)) (Point3D 0.5 0.05 (-0.45))
              box (Point3D (-0.5) 0 0.5) (Point3D 0.5 0.05 0.45)
              box (Point3D (-0.5) 0 0.5) (Point3D (-0.45) 0.05 (-0.5))
              box (Point3D 0.5 0 0.5) (Point3D 0.45 0.05 (-0.5))
              material $ pigment $ pure tan
terrainTile s q = basicTerrainTile s q

-- |
-- Answers the height of a type of terrain.
-- Note that, with some exceptions, all terrain is the same height, but some terrain is given
-- sharper contrast in its normal vectors than others (see 'terrainTileShape').
-- Unrecognized terrain types will appear very tall, so they can be easily noticed and corrected.
--
terrainHeight :: B.ByteString -> RSdouble
terrainHeight "water" = 0.01
terrainHeight "deepwater" = 0.005
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
terrainHeight "rockface" = 1.0
terrainHeight "downstairs" = 0.01
terrainHeight _ = 5.0

-- |
-- Answers the material of a type of terrain.
-- Unrecognized terrain types will appear bright magenta, so they can be easily
-- noticed and corrected.
--
terrainTexture :: B.ByteString -> MaterialM () ()
terrainTexture "water" =
    do pigment $ pure blue
       specular 100 $ pure white
terrainTexture "deepwater" =
    do pigment $ pure royal_blue
       specular 100 $ pure white
terrainTexture "sand" = pigment $ pure beige
terrainTexture "desert" = pigment $ pure $ lerp 0.5 (light_brown,white)
terrainTexture "grass" = pigment $ pure green
terrainTexture "dirt" = pigment $ pure brown
terrainTexture "forest" = pigment $ pure olive_green
terrainTexture "deepforest" = pigment $ pure olive_green
terrainTexture "ice" =
    do pigment $ pure white
       specular 1 $ pure teal
terrainTexture "lava" =
    do pigment $ pure blackbody
       emissive $ pure red
terrainTexture "glass" =
    do pigment $ pure black
       specular 1 $ pure white
terrainTexture "rockyground" = pigment $ pure grey
terrainTexture "rubble" = pigment $ pure grey
terrainTexture "rockface" = pigment $ pure grey
terrainTexture "downstairs" = pigment $ pure blackbody
terrainTexture _ =
    do pigment $ pure blackbody
       emissive $ pure magenta

