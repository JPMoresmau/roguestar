
module PlaneData
    (Plane(..))
    where

import TerrainData

data Plane = Plane
    { plane_terrain :: TerrainMap }
    deriving (Read,Show)
