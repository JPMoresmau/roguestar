{-# LANGUAGE PatternGuards, FlexibleContexts, ScopedTypeVariables #-}

module PlaneVisibility
    (dbGetVisibleTerrainForFaction,
     dbGetVisibleObjectsForFaction)
    where

import FactionData
import DB
import TerrainData
import Plane
import PlaneData
import Control.Monad
import CreatureData
import Data.List
import Grids
import GridRayCaster
import VisibilityData
import Facing
import Data.Ratio
import Building
import Position

dbGetSeersForFaction :: (DBReadable db) => Faction -> PlaneRef -> db [CreatureRef]
dbGetSeersForFaction faction plane_ref = 
    filterRO (filterByFaction faction) =<< dbGetContents plane_ref

-- |
-- Returns a list of all terrain patches that are visible to any creature belonging
-- to the specified faction on the specified plane.
--
dbGetVisibleTerrainForFaction :: (DBReadable db) => Faction -> PlaneRef -> 
                                                    db [(TerrainPatch,Position)]
dbGetVisibleTerrainForFaction faction plane_ref =
    do critters <- dbGetSeersForFaction faction plane_ref
       liftM (nub . concat) $ mapRO dbGetVisibleTerrainForCreature critters

-- |
-- Returns a list of all terrain patches that are visible to the specified creature.
--
dbGetVisibleTerrainForCreature :: (DBReadable db) => CreatureRef -> db [(TerrainPatch,Position)]
dbGetVisibleTerrainForCreature creature_ref =
    do loc <- liftM (fmap location) $ getPlanarPosition creature_ref
       spot_check <- dbGetSpotCheck creature_ref
       case loc of
		Just (plane_ref,creature_at) -> liftM (visibleTerrain creature_at spot_check . plane_terrain) $ dbGetPlane plane_ref
		Nothing -> return []

-- |
-- Returns a list of all objects that are visible to any creature belonging
-- to the specified faction on the specified plane.
--
dbGetVisibleObjectsForFaction :: (DBReadable db,GenericReference a S) => Faction -> PlaneRef -> db [a]
dbGetVisibleObjectsForFaction faction plane_ref =
    do critters <- dbGetSeersForFaction faction plane_ref
       liftM (nubBy (=:=) . concat) $ mapM dbGetVisibleObjectsForCreature critters

-- |
-- Returns a list of all objects that are visible to the specified creature.
--
dbGetVisibleObjectsForCreature :: (DBReadable db,GenericReference a S) => CreatureRef -> db [a]
dbGetVisibleObjectsForCreature creature_ref =
    do (loc :: Maybe PlaneRef) <- liftM (fmap location) $ getPlanarPosition creature_ref
       case loc of
		Just plane_ref -> filterRO (dbIsPlanarVisibleTo creature_ref . generalizeReference) =<< dbGetContents plane_ref
		Nothing -> return []

-- |
-- dbIsPlanarVisibleTo (a creature) (some object) is true if the creature can see the object.
--
dbIsPlanarVisibleTo :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Bool
dbIsPlanarVisibleTo creature_ref obj_ref | creature_ref =:= obj_ref = return True
dbIsPlanarVisibleTo creature_ref obj_ref =
    do (creature_loc :: Maybe (PlaneRef,Position)) <- liftM (fmap location) $ getPlanarPosition creature_ref
       (obj_loc :: Maybe (PlaneRef,MultiPosition)) <- liftM (fmap location) $ getPlanarPosition obj_ref
       spot_check <- dbGetOpposedSpotCheck creature_ref obj_ref
       case (creature_loc,obj_loc) of
		(Nothing,_) -> return False
		(_,Nothing) -> return False
		(Just (c_plane,_),Just (o_plane,_)) | c_plane /= o_plane -> return False --never see objects on different planes
		(Just (_,cp),Just (_,ops)) | distanceBetweenChessboard cp ops <= 1 -> return True --automatically see 8-adjacent objects
		(Just (_,cp),Just (_,ops)) | distanceBetweenSquared cp ops > (maximumRangeForSpotCheck spot_check)^2 -> return False --cull objects that are too far away to ever be seen
		(Just (c_plane,cp),Just (_,ops)) -> liftM or $ forM (positionPairs cp ops) $ 
                    \(Position (cx,cy),Position (ox,oy)) ->
                        do let delta_at = (ox-cx,oy-cy)
		           terrain <- liftM plane_terrain $ dbGetPlane c_plane -- falling through all other tests, cast a ray for visibility
		           return $ castRay (cx,cy) (ox,oy) (spot_check - distanceCostForSight Here delta_at) (terrainOpacity . gridAt terrain)

dbGetOpposedSpotCheck :: (DBReadable db) => CreatureRef -> Reference a -> db Integer
dbGetOpposedSpotCheck creature_ref object_ref =
    do spot <- dbGetSpotCheck creature_ref
       hide <- dbGetHideCheck object_ref
       return $ round $ (spot%1) * opposedLinearPowerRatio spot hide

dbGetSpotCheck :: (DBReadable db) => CreatureRef -> db Integer
dbGetSpotCheck creature_ref = liftM (creatureAbilityScore SpotSkill) $ dbGetCreature creature_ref

dbGetHideCheck :: (DBReadable db) => Reference a -> db Integer
dbGetHideCheck ref | Just creature_ref <- coerceReferenceTyped _creature ref = liftM (creatureAbilityScore HideSkill) $ dbGetCreature creature_ref
dbGetHideCheck ref | Just building_ref <- coerceReferenceTyped _building ref = liftM negate $ buildingSize building_ref
dbGetHideCheck _   | otherwise = return 1

-- |
-- visibleTerrain (creature's location) (spot check) (the terrain map) gives
-- a list of visible terrain patches from that location with that spot check.
--
visibleTerrain :: Position -> Integer -> TerrainGrid -> [(TerrainPatch,Position)]
visibleTerrain (Position (creature_at@(creature_x,creature_y))) spot_check terrain =
    let max_range = maximumRangeForSpotCheck spot_check
	in map (\(x,y) -> (gridAt terrain (x,y),Position (x,y))) $
	   castRays creature_at
			[terrainPatchBrightnessForm creature_at spot_check (creature_x+x,creature_y+y)
			 | x <- [-max_range..max_range],
			 y <- [-max_range..max_range],
			 x^2+y^2 <= max_range^2]
			(terrainOpacity . gridAt terrain)

-- |
-- terrainPatchBrightnessForm (creature's location) (spot check) (terrain patch's location)
-- gives (the patch's location,the patch's effective brightness) for the purpose of applying castRays.
--
terrainPatchBrightnessForm :: (Integer,Integer) -> Integer -> (Integer,Integer) -> ((Integer,Integer),Integer)
terrainPatchBrightnessForm creature_at spot_check patch_at =
    let delta_at = (fst creature_at - fst patch_at,snd creature_at - snd patch_at)
	in (patch_at,spot_check - distanceCostForSight Here delta_at)

-- |
-- Returns true if the specified CreatureRef belongs to the specified Faction.
--
filterByFaction :: (DBReadable db) => Faction -> CreatureRef -> db Bool
filterByFaction faction = liftM ((== faction) . creature_faction) . dbGetCreature
