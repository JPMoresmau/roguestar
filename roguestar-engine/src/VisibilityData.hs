
module VisibilityData
    (distanceCostForSight,
     terrainHideMultiplier,
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
terrainHideMultiplier RecreantFactory = 0
terrainHideMultiplier Downstairs = 2
terrainHideMultiplier Upstairs = 0

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
terrainOpacity RecreantFactory = 0
terrainOpacity Downstairs = 0
terrainOpacity Upstairs = 0

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
