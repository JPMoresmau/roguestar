{-# LANGUAGE OverloadedStrings #-}
module PlanetData
    (PlanetInfo(..),
     addTown,
     all_planets,
     pgto_planets)
    where

import TerrainData
import BuildingData
import Data.Ratio
import qualified Data.ByteString.Char8 as B

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
    planet_info_name :: Maybe B.ByteString,
    -- | Number of dungeon levels on the planet.
    planet_info_depth :: Integer,
    planet_info_biome :: Biome,
    planet_info_dungeon :: Biome,
    planet_info_town :: [(Rational,BuildingType)] }
        deriving (Read,Show)

pgto :: Integer -> B.ByteString -> Biome -> PlanetInfo
pgto x name biome = PlanetInfo {
    planet_info_priority = fromInteger x / 3,
    planet_info_name = case name of
                           "" -> Nothing
                           _  -> Just name,
    planet_info_depth = x,
    planet_info_biome = biome,
    planet_info_dungeon = case () of
        () | biome == OceanBiome -> AbyssalDungeon
        () | x == 1 -> ShallowDungeon
        () -> DeepDungeon,
    planet_info_town = [(1,Portal)] }

addTown :: PlanetInfo -> [(Rational,BuildingType)] -> PlanetInfo
addTown planet_info town = planet_info { planet_info_town = planet_info_town planet_info ++ town }

all_planets :: [PlanetInfo]
all_planets = concat [pgto_planets]

pgto_planets :: [PlanetInfo]
pgto_planets = [
    pgto 1 "" RockBiome,
    pgto 1 "" IcyRockBiome,
    pgto 1 "" TundraBiome,
    pgto 1 "" DesertBiome,
    pgto 1 "" MountainBiome,
    pgto 2 "roanoke" SwampBiome,
    pgto 2 "pamlico" SwampBiome,
    pgto 2 "pungo" ForestBiome,
    pgto 2 "neuse" ForestBiome,
    pgto 2 "crabtree" SwampBiome,
    pgto 2 "eno" SwampBiome `addTown` [(1%20,Monolith)],
    pgto 2 "yadkin" SwampBiome,
    pgto 2 "catawba" ForestBiome,
    pgto 2 "pasquotank" ForestBiome,
    pgto 3 "dogwood" GrasslandBiome,
    pgto 3 "emerald" GrasslandBiome,
    pgto 3 "cardinal" GrasslandBiome,
    pgto 4 "currituck" OceanBiome,
    pgto 4 "hatteras" OceanBiome,
    pgto 4 "lookout" OceanBiome,
    pgto 4 "ocracoke" OceanBiome]

