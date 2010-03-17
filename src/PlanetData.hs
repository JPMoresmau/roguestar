module PlanetData
    (PlanetInfo(..),
     addTown,
     addPriority,
     all_planets,
     pgto_planets)
    where

import TerrainData
import BuildingData
import Data.Ratio

-- | Information used to construct new planets.
-- Whenever the player goes through a stargate to a new planet,
-- we pull a new 'PlanetInfo' record off of a stack and a construct
-- a planet based on that information.
--
-- 'PlanetInfo's are sorted by their 'planet_info_priority' fields.
--
data PlanetInfo = PlanetInfo {
    -- | Between 0 and 1 are randomly added to this value, and then all 'PlanetInfo's are sorted by priority.
    -- This gives the order in which players visit planets.
    planet_info_priority :: Double,
    -- | Some planets have names.
    planet_info_name :: Maybe String,
    planet_info_biome :: Biome,
    planet_info_town :: [(Rational,BuildingType)] }
        deriving (Read,Show)

pgto :: String -> Biome -> PlanetInfo
pgto name biome = PlanetInfo {
    planet_info_priority = 0.0,
    planet_info_name = Just name,
    planet_info_biome = biome,
    planet_info_town = [(1,Portal)] }

addTown :: PlanetInfo -> [(Rational,BuildingType)] -> PlanetInfo
addTown planet_info town = planet_info { planet_info_town = planet_info_town planet_info ++ town }

addPriority :: PlanetInfo -> Double -> PlanetInfo
addPriority planet_info prio = planet_info { planet_info_priority = planet_info_priority planet_info + prio }

all_planets :: [PlanetInfo]
all_planets = concat [pgto_planets]

pgto_planets :: [PlanetInfo]
pgto_planets = [
    pgto "roanoke" SwampBiome,
    pgto "pamlico" SwampBiome,
    pgto "pungo" ForestBiome,
    pgto "neuse" ForestBiome,
    pgto "crabtree" SwampBiome,
    pgto "eno" SwampBiome `addTown` [(1%20,Monolith)],
    pgto "yadkin" SwampBiome,
    pgto "catawba" ForestBiome,
    pgto "pasquotank" ForestBiome,
    pgto "dogwood" GrasslandBiome `addPriority` 0.75,
    pgto "emerald" GrasslandBiome `addPriority` 0.75,
    pgto "cardinal" GrasslandBiome `addPriority` 0.75,
    pgto "currituck" OceanBiome `addPriority` 1.5,
    pgto "hatteras" OceanBiome `addPriority` 1.5,
    pgto "lookout" OceanBiome `addPriority` 1.5,
    pgto "ocracoke" OceanBiome `addPriority` 1.5]
