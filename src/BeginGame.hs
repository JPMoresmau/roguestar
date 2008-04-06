
module BeginGame
    (dbBeginGame)
    where

import Plane
import CreatureData
import Character
import CharacterData
import DB
import DBData
import Facing
import TerrainData
import Data.Maybe
import ToolData

player_race_to_biome :: [(String,Biome)]
player_race_to_biome =
    [("anachronid",DesertBiome),
     ("androsynth",RockBiome),
     ("ascendant",MountainBiome),
     ("canduceator",SwampBiome),
     ("encephalon",GrasslandBiome{-SwampBiome-}),
     ("goliath",DesertBiome),
     ("hellion",GrasslandBiome),
     ("kraken",OceanBiome),
     ("myrmidon",DesertBiome),
     ("perennial",ForestBiome),
     ("recreant",DesertBiome),
     ("reptilian",SwampBiome)]

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    do seed <- dbNextRandomInteger
       dbNewPlane $ TerrainGenerationData {
           tg_smootheness = 3,
	   tg_biome = fromMaybe GrasslandBiome $ lookup (creature_species_name creature) player_race_to_biome,
	   tg_placements = [recreantFactories seed] }

-- |
-- Begins the game with the specified starting player creature and the specified starting character class.
-- The character class should not be pre-applied to the creature.
--
dbBeginGame :: Creature -> CharacterClass -> DB ()
dbBeginGame creature character_class = 
    do let first_level_creature = applyCharacterClass character_class creature
       plane_ref <- dbCreateStartingPlane creature
       landing_site <- pickRandomClearSite 200 30 2 (Position (0,0)) (not . (`elem` difficult_terrains)) plane_ref
       creature_ref <- dbAddCreature first_level_creature (Standing plane_ref landing_site Here)
       phaser_position <- pickRandomClearSite 200 1 2 landing_site (not . (`elem` difficult_terrains)) plane_ref
       dbAddTool phase_pistol (Dropped plane_ref phaser_position)
       dbSetState $ DBPlayerCreatureTurn creature_ref DBNormal
