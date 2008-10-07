
module PlaneData
    (Plane(..))
    where

import TerrainData

data Plane = Plane
    { plane_biome :: Biome,
      plane_terrain :: TerrainGrid }
    deriving (Read,Show)
