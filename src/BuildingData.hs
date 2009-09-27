
module BuildingData
    (Building(..),
     BuildingType(..),
     buildingOccupies)
    where

data Building = Building
        deriving (Read,Show)

data BuildingType = Monolith | Stargate
        deriving (Eq,Ord,Read,Show)

-- | Get a list of squares, relative to the center of the building (0,0),
-- that a building occupies.  These squares must be free of unfriendly terrain
-- (mountains, trees, water, lava, etc.) and no other objects can co-occupy these squares.
--
-- A goal is that every building type has a unique square occupation signature,
-- so that it can be identified by it's shape alone.
buildingOccupies :: BuildingType -> [(Integer,Integer)]
-- Monolith:   X
buildingOccupies Monolith = [(0,0)]
-- Stargate:  XXX
buildingOccupies Stargate = [(0,0),(-1,0),(1,0)]

