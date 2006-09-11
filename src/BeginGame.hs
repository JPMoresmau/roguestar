module BeginGame
    (dbBeginGame)
    where

import Plane
import CreatureData
import Character
import CharacterData
import DB
import DBData
import TerrainData
import Data.Maybe

player_race_to_biome :: [(String,Biome)]
player_race_to_biome =
    [("anachronid",DeasertBiome),
     ("androsynth",RockBiome),
     ("ascendant",MountainBiome),
     ("canduceator",SwampBiome),
     ("encephalon",SwampBiome),
     ("goliath",DeasertBiome),
     ("hellion",GrasslandBiome),
     ("kraken",OceanBiome),
     ("myrmidon",DeasertBiome),
     ("perennial",ForestBiome),
     ("recreant",DeasertBiome),
     ("reptilian",SwampBiome)]

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    dbNewPlane $ TerrainGenerationData {tg_smootheness = 5,
					tg_biome = fromMaybe GrasslandBiome $ lookup (creature_species_name creature) player_race_to_biome}

-- |
-- Begins the game with the specified starting player creature and the specified starting character class.
-- The character class should not be pre-applied to the creature.
--
dbBeginGame :: Creature -> CharacterClass -> DB ()
dbBeginGame creature character_class = 
    do let first_level_creature = applyCharacterClass character_class creature
       creature_ref <- dbAddCreature first_level_creature 
       plane_ref <- dbCreateStartingPlane creature
       landing_site <- pickRandomClearSite 200 30 2 plane_ref
       dbMoveInto plane_ref creature_ref (DBCoordinateLocation landing_site)
       dbSetState $ DBPlayerCreatureTurn creature_ref