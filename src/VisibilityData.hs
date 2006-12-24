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

module VisibilityData
    (distanceCostForSight,
     terrainHideMultiplier,
     terrainSpotMultiplier,
     terrainOpacity,
     maximumRangeForSpotCheck)
    where

import TerrainData
import Data.List
import Facing

-- |
-- We multiply a creature's hide check by this number if it is standing on this terrain.
--
terrainHideMultiplier :: TerrainPatch -> Integer
terrainHideMultiplier RockFace = 3
terrainHideMultiplier Rubble = 2
terrainHideMultiplier (Ore {}) = 2
terrainHideMultiplier RockyGround = 1
terrainHideMultiplier Dirt = 0
terrainHideMultiplier Grass = 1
terrainHideMultiplier Sand = 1
terrainHideMultiplier Desert = 1
terrainHideMultiplier Forest = 2
terrainHideMultiplier DeepForest = 2
terrainHideMultiplier Water = 2
terrainHideMultiplier DeepWater = 2
terrainHideMultiplier Ice = 0
terrainHideMultiplier Lava = 0  -- you definitely can't hide on lava
terrainHideMultiplier Glass = 0

-- |
-- We multiply a creature's spot check by this number if it is standing on this terrain.
--
terrainSpotMultiplier :: TerrainPatch -> Integer
terrainSpotMultiplier RockFace = 3
terrainSpotMultiplier _ = 1

-- |
-- We cast a ray between the spotter and the hider.  This indicates to what extent each terrain type
-- interferes with vision.
--
terrainOpacity :: TerrainPatch -> Integer
terrainOpacity RockFace = 90
terrainOpacity Rubble = 10
terrainOpacity (Ore {}) = 10
terrainOpacity RockyGround = 0
terrainOpacity Dirt = 0
terrainOpacity Grass = 5
terrainOpacity Sand = 0
terrainOpacity Desert = 0
terrainOpacity Forest = 25
terrainOpacity DeepForest = 50
terrainOpacity Water = 0
terrainOpacity DeepWater = 0
terrainOpacity Ice = 0
terrainOpacity Lava = 0
terrainOpacity Glass = 0

-- |
-- The difficulty to spot an object at the given relative coordinates, taking facing into account.
--
distanceCostForSight :: Facing -> (Integer,Integer) -> Integer
distanceCostForSight facing (x,y) =
    let (xface,yface) = facingToRelative facing
	(x',y') = (x-xface,y-yface)
	in (x*x' + y*y')

-- |
-- The maximum distance from any point that a creature with that spot check could see anything,
-- no matter how well lit.
--
maximumRangeForSpotCheck :: Integer -> Integer
maximumRangeForSpotCheck spot_check = genericLength $ takeWhile (< spot_check) [((x+1)*(x+1)) | x <- [1..]]