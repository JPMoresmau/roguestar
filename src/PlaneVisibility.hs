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
	   filter ( \ (x,y) -> terrainPatchIsVisible creature_at (x,y) spot_check terrain)
		      [(x,y) | x <- [creature_x-max_range..creature_x+max_range], 
		       y <- [creature_y-max_range..creature_y+max_range]]

terrainPatchIsVisible :: (Integer,Integer) -> (Integer,Integer) -> Integer -> TerrainMap -> Bool
terrainPatchIsVisible creature_at patch_at spot_check terrain =
    let delta_at = (fst creature_at - fst patch_at,snd creature_at - snd patch_at)
	in castRay creature_at patch_at 
	       (spot_check - distanceCostForSight Here delta_at) $
	       terrainOpacity . gridAt terrain

-- |
-- The maximum distance from any point that a creature with that spot check could see anything,
-- no matter how well lit.
--
maximumRangeForSpotCheck :: Integer -> Integer
maximumRangeForSpotCheck spot_check = genericLength $ takeWhile (< spot_check) [x*x | x <- [1..]]