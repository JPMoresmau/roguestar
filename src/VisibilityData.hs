module VisibilityData
    (Facing(..),
     distanceCostForSight,
     terrainHideMultiplier,
     terrainSpotMultiplier,
     terrainOpacity,
     facingToRelative7,
     maximumRangeForSpotCheck)
    where

import TerrainData
import Data.List

data Facing = North
	    | NorthEast
	    | East
	    | SouthEast
	    | South
	    | SouthWest
	    | West
	    | NorthWest
	    | Here
	      deriving (Eq,Enum,Bounded)
-- |
-- In relative coordinates, one integral step in the specified direction.
--
facingToRelative :: Facing -> (Integer,Integer)
facingToRelative North = (0,1)
facingToRelative NorthEast = (1,1)
facingToRelative East = (1,0)
facingToRelative SouthEast = (1,-1)
facingToRelative South = (0,-1)
facingToRelative SouthWest = (-1,-1)
facingToRelative West = (-1,0)
facingToRelative NorthWest = (-1,1)
facingToRelative Here = (0,0)

-- |
-- In relative coordinates, roughly seven integral steps in the specified direction.
-- Note: 7 is a small integer such that the square root of 5^2 is quite close to 7.
--
facingToRelative7 :: Facing -> (Integer,Integer)
facingToRelative7 North = (0,7)
facingToRelative7 NorthEast = (5,5)
facingToRelative7 East = (7,0)
facingToRelative7 SouthEast = (5,-5)
facingToRelative7 South = (0,-7)
facingToRelative7 SouthWest = (-5,-5)
facingToRelative7 West = (-7,0)
facingToRelative7 NorthWest = (-5,5)
facingToRelative7 Here = (0,0)

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
terrainHideMultiplier (DungeonEntrance {}) = 0
terrainHideMultiplier DungeonExit = 0

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
terrainOpacity RockFace = 100
terrainOpacity Rubble = 1
terrainOpacity (Ore {}) = 1
terrainOpacity RockyGround = 0
terrainOpacity Dirt = 0
terrainOpacity Grass = 0
terrainOpacity Sand = 0
terrainOpacity Desert = 0
terrainOpacity Forest = 2
terrainOpacity DeepForest = 5
terrainOpacity Water = 0
terrainOpacity DeepWater = 0
terrainOpacity Ice = 0
terrainOpacity Lava = 0
terrainOpacity Glass = 0
terrainOpacity (DungeonEntrance {}) = 0
terrainOpacity DungeonExit = 0

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
maximumRangeForSpotCheck spot_check = genericLength $ takeWhile (< spot_check) [(x*x) | x <- [1..]]