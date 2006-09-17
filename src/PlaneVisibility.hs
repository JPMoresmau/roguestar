module PlaneVisibility
    (dbGetVisibleTerrainForFaction,
     dbGetVisibleTerrainForCreature)
    where

import FactionData
import DBData
import DB
import TerrainData
import Plane
import PlaneData
import Control.Monad
import CreatureData
import Data.Maybe
import Data.List
import Grids
import GridRayCaster
import VisibilityData
import Data.Set as Set (fromList,toList)

dbGetVisibleTerrainForFaction :: Faction -> PlaneRef -> DB [((Integer,Integer),TerrainPatch)]
dbGetVisibleTerrainForFaction faction plane_ref = 
    do critters <- liftM (map fst) $ dbGetCreaturesFiltered plane_ref filterByFaction
       liftM (Set.toList . Set.fromList . concat) $ mapM dbGetVisibleTerrainForCreature critters
	   where filterByFaction = liftM ((== faction) . creature_faction) . dbGetCreature

dbGetVisibleTerrainForCreature :: CreatureRef -> DB [((Integer,Integer),TerrainPatch)]
dbGetVisibleTerrainForCreature creature_ref =
    do loc <- dbGetPlanarLocation creature_ref
       spot_check <- liftM (creatureScore Spot) $ dbGetCreature creature_ref
       case loc of
		Just (plane_ref,creature_at) -> liftM (visibleTerrain creature_at spot_check . plane_terrain) $ dbGetInstancedPlane plane_ref 
		Nothing -> return []

visibleTerrain :: (Integer,Integer) -> Integer -> TerrainMap -> [((Integer,Integer),TerrainPatch)]
visibleTerrain creature_at@(creature_x,creature_y) spot_check terrain =
    let max_range = maximumRangeForSpotCheck spot_check
	in map ( \ (x,y) -> ((x,y), gridAt terrain (x,y))) $
	   castRays creature_at 
			[terrainPatchBrightnessForm creature_at spot_check (creature_x+x,creature_y+y) 
			 | x <- [-max_range..max_range], 
			 y <- [-max_range..max_range],
			 x^2+y^2 <= max_range^2]
			(terrainOpacity . gridAt terrain)

terrainPatchBrightnessForm :: (Integer,Integer) -> Integer -> (Integer,Integer) -> ((Integer,Integer),Integer)
terrainPatchBrightnessForm creature_at spot_check patch_at =
    let delta_at = (fst creature_at - fst patch_at,snd creature_at - snd patch_at)
	in (patch_at,spot_check - distanceCostForSight Here delta_at)
