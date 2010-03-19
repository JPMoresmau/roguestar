
module PlaneData
    (Plane(..))
    where

import TerrainData

data Plane = Plane
    { plane_biome :: Biome,
      plane_terrain :: TerrainGrid,
      plane_random_id :: Integer,
      plane_planet_name :: String }
    deriving (Read,Show)
