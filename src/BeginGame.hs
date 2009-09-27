
module BeginGame
    (dbBeginGame)
    where

import Plane
import CreatureData
import Character
import CharacterData
import BuildingData
import DB
import DBData
import Facing
import TerrainData
import ToolData
import Control.Monad
import Control.Monad.Random
import SpeciesData
import Substances
import PlayerState
import Town

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

startingEquipmentByClass :: CharacterClass -> [Tool]
startingEquipmentByClass Barbarian = [kinetic_fleuret]
startingEquipmentByClass Consular = [sphere Silver]
startingEquipmentByClass Engineer = [sphere Crudnium,sphere Molybdenum,sphere Uranium]
startingEquipmentByClass ForceAdept = [kinetic_sabre]
startingEquipmentByClass Marine = [phase_pistol,phase_rifle]
startingEquipmentByClass Ninja = []
startingEquipmentByClass Pirate = [phaser]
startingEquipmentByClass Scout = [phase_pistol]
startingEquipmentByClass Shepherd = [sphere Wood]
startingEquipmentByClass Thief = [sphere Platinum]
startingEquipmentByClass Warrior = [phaser,kinetic_fleuret]

startingEquipmentBySpecies :: Species -> [Tool]
startingEquipmentBySpecies Anachronid = [sphere Radon]
startingEquipmentBySpecies Ascendant = [sphere Neon]
startingEquipmentBySpecies Androsynth = [sphere Silicon]
startingEquipmentBySpecies Caduceator = [sphere Silver]
startingEquipmentBySpecies Encephalon = [sphere Ammonia]
startingEquipmentBySpecies Goliath = [sphere Iron]
startingEquipmentBySpecies Hellion = [sphere Methane]
startingEquipmentBySpecies Kraken = [sphere Substances.Water]
startingEquipmentBySpecies Myrmidon = [sphere Krypton]
startingEquipmentBySpecies Perennial = [sphere Wood]
startingEquipmentBySpecies Recreant = [sphere Malignite]
startingEquipmentBySpecies Reptilian = [sphere Oxygen]

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    do seed <- getRandom
       dbNewPlane $ TerrainGenerationData {
           tg_smootheness = 3,
	   tg_biome = homeBiome $ creature_species creature,
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
       createTown plane_ref [Stargate,Monolith]
       let starting_equip = startingEquipmentBySpecies (creature_species creature) ++ startingEquipmentByClass character_class
       forM_ starting_equip $ \tool -> dbAddTool tool (Inventory creature_ref)
       forM_ [0..10] $ \_ -> do tool_position <- pickRandomClearSite 200 1 2 landing_site (not . (`elem` difficult_terrains)) plane_ref
                                tool_type <- weightedPickM [(8,phase_pistol),(5,phaser),(3,phase_rifle),(8,kinetic_fleuret),(3,kinetic_sabre),
                                                              (5,Sphere $ toSubstance Nitrogen),(5,Sphere $ toSubstance Ionidium),(5,Sphere $ toSubstance Aluminum)]
                                dbAddTool tool_type (Dropped plane_ref tool_position)
       setPlayerState $ PlayerCreatureTurn creature_ref NormalMode
