
module BuildingData
    (Building(..),
     BuildingType(..),
     NodeType(..),
     all_nodes,
     showBuilding,
     buildingOccupies)
    where

data Building = Building
        deriving (Read,Show)

data BuildingType = Node NodeType | Portal
        deriving (Eq,Ord,Read,Show)

data NodeType = Monolith | Anchor
        deriving (Eq,Ord,Read,Show,Enum,Bounded)

all_nodes :: [NodeType]
all_nodes = [minBound..maxBound]

showBuilding :: BuildingType -> String
showBuilding (Node n) = show n
showBuilding x = show x

-- | Get a list of squares, relative to the center of the building (0,0),
-- that a building occupies.  These squares must be free of unfriendly terrain
-- (mountains, trees, water, lava, etc.) and no other objects can co-occupy these squares.
--
-- A goal is that every building type has a unique occupation signature,
-- so that it can be identified by it's shape alone.
buildingOccupies :: BuildingType -> [(Integer,Integer)]
-- Monolith:  X
buildingOccupies (Node _) = [(0,0)]
-- Portal:   XXX
buildingOccupies Portal = [(0,0),(-1,0),(1,0)]

