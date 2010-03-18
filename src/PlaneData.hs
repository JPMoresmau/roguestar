
module PlaneData
    (Plane(..))
    where

import TerrainData
import PlanetData

data Plane = Plane
    { plane_biome :: Biome,
      plane_terrain :: TerrainGrid,
      plane_planet_name :: String }
    deriving (Read,Show)
