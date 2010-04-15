
module PlaneData
    (Plane(..))
    where

import TerrainData
import qualified Data.ByteString.Char8 as B

data Plane = Plane
    { plane_biome :: Biome,
      plane_terrain :: TerrainGrid,
      plane_random_id :: Integer,
      plane_planet_name :: B.ByteString }
    deriving (Read,Show)
