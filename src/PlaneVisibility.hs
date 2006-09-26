--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module PlaneVisibility
    (dbGetVisibleTerrainForFaction,
     dbGetVisibleTerrainForCreature,
     dbGetVisibleObjectsForFaction)
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
import Facing

-- |
-- Returns a list of all terrain patches that are visible to any creature belonging
-- to the specified faction on the specified plane.
--
dbGetVisibleTerrainForFaction :: Faction -> PlaneRef -> DB [((Integer,Integer),TerrainPatch)]
dbGetVisibleTerrainForFaction faction plane_ref = 
    do critters <- liftM (map fst) $ dbGetCreaturesFiltered plane_ref (filterByFaction faction)
       liftM (Set.toList . Set.fromList . concat) $ mapM dbGetVisibleTerrainForCreature critters

-- |
-- Returns a list of all terrain patches that are visible to the specified creature.
--
dbGetVisibleTerrainForCreature :: CreatureRef -> DB [((Integer,Integer),TerrainPatch)]
dbGetVisibleTerrainForCreature creature_ref =
    do loc <- dbGetPlanarLocation creature_ref
       spot_check <- dbGetSpotCheck creature_ref
       case loc of
		Just (plane_ref,creature_at) -> liftM (visibleTerrain creature_at spot_check . plane_terrain) $ dbGetInstancedPlane plane_ref 
		Nothing -> return []

-- |
-- Returns a list of all objects that are visible to any creature belonging
-- to the specified faction on the specified plane.
--
dbGetVisibleObjectsForFaction :: Faction -> PlaneRef -> DB [(DBReference,(Integer,Integer))]
dbGetVisibleObjectsForFaction faction plane_ref =
    do critters <- liftM (map fst) $ dbGetCreaturesFiltered plane_ref (filterByFaction faction)
       liftM (nub . concat) $ mapM dbGetVisibleObjectsForCreature critters

-- |
-- Returns a list of all objects that are visible to the specified creature.
--
dbGetVisibleObjectsForCreature :: CreatureRef -> DB [(DBReference,(Integer,Integer))]
dbGetVisibleObjectsForCreature creature_ref =
    do loc <- dbGetPlanarLocation creature_ref
       case loc of
		Just (plane_ref,_) -> filterM (dbIsPlanarVisibleTo creature_ref . fst) =<< (liftM toCoordinateLocationForm $ dbGetContents plane_ref)
		Nothing -> return []

dbIsPlanarVisibleTo :: CreatureRef -> DBReference -> DB Bool
dbIsPlanarVisibleTo creature_ref obj_ref =
    do creature_loc <- dbGetPlanarLocation creature_ref
       obj_loc <- dbGetPlanarLocation obj_ref
       spot_check <- liftM2 (-) (dbGetSpotCheck creature_ref) (dbGetHideCheck obj_ref)
       case (creature_loc,obj_loc) of
				   (Nothing,_) -> return False
				   (_,Nothing) -> return False
				   (Just (c_plane,_),Just (o_plane,_)) | c_plane /= o_plane -> return False
				   (Just (_,(cx,cy)),Just (_,(ox,oy))) | (ox-cx)^2+(oy-cy)^2 > maximumRangeForSpotCheck spot_check -> return False
				   (Just (c_plane,c_at),Just (_,o_at)) -> do terrain <- liftM plane_terrain $ dbGetInstancedPlane c_plane
									     return $ castRay c_at o_at spot_check (terrainOpacity . gridAt terrain)

dbGetSpotCheck :: CreatureRef -> DB Integer
dbGetSpotCheck creature_ref = liftM (creatureScore Spot) $ dbGetCreature creature_ref

dbGetHideCheck :: DBReference -> DB Integer
dbGetHideCheck (DBCreatureRef creature_ref) = liftM (creatureScore Hide) $ dbGetCreature creature_ref
dbGetHideCheck _ = return 0

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

filterByFaction :: Faction -> CreatureRef -> DB Bool
filterByFaction faction = liftM ((== faction) . creature_faction) . dbGetCreature