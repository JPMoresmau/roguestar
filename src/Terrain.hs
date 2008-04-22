{-# LANGUAGE PatternSignatures, FlexibleContexts #-}

module Terrain
    (terrainAt,
     whatIsOccupying,
     isTerrainPassable)
    where

import TerrainData
import DB
import Control.Monad
import PlaneData
import Grids
import Data.Maybe

terrainAt :: (DBReadable db) => PlaneRef -> Position -> db TerrainPatch
terrainAt plane_ref (Position (x,y)) =
    do terrain <- liftM plane_terrain $ dbGetPlane plane_ref
       return $ gridAt terrain (x,y)

whatIsOccupying :: (DBReadable db,GenericReference a S) => PlaneRef -> Position -> db [a]
whatIsOccupying plane_ref position =
    liftM (mapMaybe fromLocation . filter ((== position) . location) . map (asLocationTyped _nullary _position)) $ dbGetContents plane_ref

isTerrainPassable :: (DBReadable db) => PlaneRef -> CreatureRef -> Position -> db Bool
isTerrainPassable plane_ref creature_ref position = 
    do (critters :: [CreatureRef]) <- liftM (filter (/= creature_ref)) $ whatIsOccupying plane_ref position
       terrain <- terrainAt plane_ref position
       return $ not (terrain `elem` [RockFace,Forest,DeepForest]) && null critters
