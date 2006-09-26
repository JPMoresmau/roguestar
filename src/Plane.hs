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

module Plane
    (dbNewPlane,
     dbGetCurrentPlane,
     dbGetInstancedPlane,
     pickRandomClearSite,
     dbGetPlanarLocation,
     dbGetEnclosingPlane)
    where

import Grids
import Dice
import DB
import DBData
import TerrainData
import PlaneData

dbNewPlane :: TerrainGenerationData -> DB PlaneRef
dbNewPlane tg_data = dbAddPlane $ Left $ UninstancedPlane tg_data

-- |
-- Recursively searches upward (through parents) for an object that is in a plane, and answers
-- the object's 
dbGetPlanarLocation :: DBRef a => a -> DB (Maybe (PlaneRef,(Integer,Integer)))
dbGetPlanarLocation object_ref =
    do parent_info <- dbWhere object_ref
       case parent_info of
			Just (DBPlaneRef plane_ref,DBCoordinateLocation location) -> return $ Just (plane_ref,location)
			Just (DBPlaneRef plane_ref,DBCoordinateFacingLocation location) -> return $ Just (plane_ref,fst location)
			Just (someplace,_) -> dbGetPlanarLocation someplace
			Nothing -> return Nothing

dbGetEnclosingPlane :: DBRef a => a -> DB (Maybe PlaneRef)
dbGetEnclosingPlane object_ref = do parent_plane_info <- dbGetPlanarLocation object_ref 
				    case parent_plane_info of
							   Just (plane_ref,_) -> return $ Just plane_ref
							   Nothing -> return Nothing

-- |
-- Gets the current plane of interest based on whose turn it is.
--
dbGetCurrentPlane :: DB (Maybe PlaneRef)
dbGetCurrentPlane = 
    do state <- dbState
       case state of
		  DBPlayerCreatureTurn creature_ref -> dbGetEnclosingPlane creature_ref
		  _ -> return Nothing

dbGetInstancedPlane :: PlaneRef -> DB InstancedPlane
dbGetInstancedPlane ref = do rns <- dbNextRandomIntegerStream
			     dbModPlane (instantiatePlane rns) ref
			     plane <- dbGetPlane ref
			     return $ either (\_ -> error "plane should be instanced") id plane

instantiatePlane :: [Integer] -> Plane -> Plane
instantiatePlane _ instanced_plane@(Right {}) = instanced_plane
instantiatePlane rns (Left uninstanced_plane) =
    Right InstancedPlane {
			  plane_terrain = generateTerrain (plane_tg_data uninstanced_plane) rns
			 }

-- |
-- Selects sites at random until one seems reasonably clear.  It begins at
-- the center (0,0) of the map, and then picks more sites further and further away from the center
-- until it one seems clear -- this tends to concentrate sites near the center.
--
-- A site is considered clear if there are no objects at all within object_clear squares, and
-- no difficult terrain (mountain,water,lava,etc) within terrain_clear squares.
--
pickRandomClearSite :: Integer -> Integer -> Integer -> PlaneRef -> DB (Integer,Integer)
pickRandomClearSite = pickRandomClearSite_ 0

pickRandomClearSite_ :: Integer -> Integer -> Integer -> Integer -> PlaneRef -> DB (Integer,Integer)
pickRandomClearSite_ search_diameter max_tries object_clear terrain_clear plane_ref =
    do x0 <- 1 `d` (2*search_diameter+1)
       y0 <- 1 `d` (2*search_diameter+1)
       x' <- return $ x0 - search_diameter - 1
       y' <- return $ y0 - search_diameter - 1
       plane <- dbGetInstancedPlane plane_ref
       terrain <- return $ plane_terrain plane
       terrain_is_clear <- return $ all (not . (`elem` difficult_terrains)) $
			   concat [[gridAt terrain (x,y) | x <- [x'-terrain_clear..x'+terrain_clear]] |
				   y <- [y'-terrain_clear..y'+terrain_clear]]
       case terrain_is_clear of
			     True -> return (x',y')
			     False | search_diameter == max_tries -> pickRandomClearSite_ 0 max_tries (max 0 $ object_clear - 1) (max 0 terrain_clear - 1) plane_ref
			     False -> pickRandomClearSite_ (search_diameter + 1) max_tries object_clear terrain_clear plane_ref