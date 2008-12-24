
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
import ToolData
import Control.Monad.Random
import SpeciesData

homeBiome :: Species -> Biome
homeBiome Anachronid = ForestBiome
homeBiome Ascendant = MountainBiome
homeBiome Androsynth = IcyRockBiome
homeBiome Caduceator = GrasslandBiome
homeBiome Encephalon = SwampBiome
homeBiome Goliath = DesertBiome
homeBiome Hellion = SwampBiome
homeBiome Kraken = OceanBiome
homeBiome Myrmidon = DesertBiome
homeBiome Perennial = GrasslandBiome
homeBiome Recreant = TundraBiome
homeBiome Reptilian = ForestBiome

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    do seed <- getRandom
       dbNewPlane $ TerrainGenerationData {
           tg_smootheness = 3,
	   tg_biome = homeBiome $ creature_species_name creature,
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
       flip mapM_ [0..10] $ \_ -> do phaser_position <- pickRandomClearSite 200 1 2 landing_site (not . (`elem` difficult_terrains)) plane_ref
                                     phaser_type <- weightedPickM [(8,phase_pistol),(5,phaser),(3,phase_rifle),(8,kinetic_fleuret),(3,kinetic_sabre)]
                                     dbAddTool phaser_type (Dropped plane_ref phaser_position)
       setPlayerState $ PlayerCreatureTurn creature_ref NormalMode
