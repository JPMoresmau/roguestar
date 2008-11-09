
module TerrainData
    (Biome(..),
     TerrainPatch(..),
     TerrainGrid,
     TerrainGenerationData(..),
     TerrainPlacement,
     recreantFactories,
     generateTerrain,
     generateExampleTerrain,
     prettyPrintTerrain,
     difficult_terrains,
     impassable_terrains)
    where

import Grids
import Data.List as List
import Data.Map as Map
import Substances
import RNG
import Data.Ratio

-- |
-- Most automatically generated surface maps belong to a Biome, representing the kind of terrain
-- and plant life that dwells in terrain generated for the map.
--
data Biome = RockBiome
           | IcyRockBiome
           | GrasslandBiome
	   | ForestBiome
           | TundraBiome
           | DesertBiome
           | OceanBiome
           | MountainBiome
	   | SwampBiome
	     deriving (Read,Show,Eq,Ord,Enum,Bounded)

-- |
-- All static terrain elements are members of TerrainGrid
--
-- The only difference between "Deasert" and "Sand" is that where
-- "Deasert" and "Water" touch, the map generator will produce
-- patches of plantlife (for oasis and shoreline effect).
--
data TerrainPatch = RockFace
                  | Rubble
                  | Ore Solid
                  | RockyGround
                  | Dirt
                  | Grass
                  | Sand
                  | Desert -- exactly like sand, except from the terrain generator's point of view: oasis can appear
                  | Forest
                  | DeepForest
                  | Water
                  | DeepWater
                  | Ice
		  | Lava
		  | Glass -- what sand becomes when struck by intense heat
		  | RecreantFactory
                    deriving (Read,Show,Eq,Ord)

data TerrainGenerationData = TerrainGenerationData
			   { tg_smootheness :: Integer,
			     tg_biome :: Biome,
			     tg_placements :: [TerrainPlacement] }
			   deriving (Read,Show)

data TerrainPlacement = TerrainPlacement {
    placement_sources :: [(Rational,TerrainPatch)],
    placement_replacements :: [(Integer,TerrainPatch)],
    placement_seed :: Integer }
        deriving (Read,Show)

placeTerrain :: TerrainPlacement -> TerrainGrid -> TerrainGrid
placeTerrain terrain_placement =
    arbitraryReplaceGrid (placement_sources terrain_placement)
                         (placement_replacements terrain_placement)
			 (placement_seed terrain_placement)

recreantFactories :: Integer -> TerrainPlacement
recreantFactories seed = TerrainPlacement {
    placement_sources = 
        [(1%25,Ice),
         (1%100,Sand),
         (1%25,Desert),
         (1%50,Dirt),
         (1%10,Glass),
	 (1%200,Grass),
         (1%1000,Forest),
         (1%25,RockyGround)],
    placement_replacements = 
        [(1,RecreantFactory)],
    placement_seed = seed }

-- |
-- A list of TerrainPatches that are considered "difficult", either for traveling
-- or for constructing buildings.
--
difficult_terrains :: [TerrainPatch]
difficult_terrains = impassable_terrains ++
                     [Water,DeepWater,Ice,Lava,RecreantFactory]

-- |
-- A list of TerrainPatches that are considered "impassable" for traveling.
--
impassable_terrains :: [TerrainPatch]
impassable_terrains = [RockFace,Forest,DeepForest]

terrainFrequencies :: Biome -> [(Integer,TerrainPatch)]
terrainFrequencies RockBiome = [(15,RockFace),(15,Rubble),(55,RockyGround),(15,Sand)]
terrainFrequencies IcyRockBiome = [(10,RockFace),(10,Rubble),(20,RockyGround),(60,Ice)]
terrainFrequencies GrasslandBiome = [(5,RockFace),(5,RockyGround),(10,Dirt),(10,Sand),(10,Forest),(10,Water),(50,Grass)]
terrainFrequencies ForestBiome = [(10,RockFace),(10,RockyGround),(10,Dirt),(10,Water),(10,Grass),(25,Forest),(25,DeepForest)]
terrainFrequencies TundraBiome = [(10,RockFace),(10,RockyGround),(10,Sand),(10,Water),(60,Ice)]
terrainFrequencies DesertBiome = [(10,RockFace),(10,RockyGround),(9,Grass),(1,Water),(70,Desert)]
terrainFrequencies OceanBiome = [(5,RockyGround),(10,Sand),(5,Grass),(5,Forest),(25,Water),(50,DeepWater)]
terrainFrequencies MountainBiome = [(50,RockFace),(25,RockyGround),(5,Rubble),(5,Sand),(5,Grass),(5,Forest),(5,Water)]
terrainFrequencies SwampBiome = [(40,Forest),(50,Water),(5,Sand),(5,Grass)]

terrainInterpFn :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpFn (a,b) = [(1,a),(1,b)] ++ (terrainInterpRule (a,b)) ++ (terrainInterpRule (b,a))

terrainInterpRule :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpRule (RockFace,RockFace) = []
terrainInterpRule (RockFace,RockyGround) = [(3,RockFace),(1,Rubble),(3,RockyGround)]
terrainInterpRule (RockFace,x) = [(3,RockFace),(2,Rubble),(1,RockyGround),(1,Sand),(7,x)]
terrainInterpRule (Rubble,x) = [(1,Rubble),(2,Sand),(2,Dirt),(5,x)]
terrainInterpRule (DeepWater,DeepWater) = []
terrainInterpRule (DeepWater,Water) = [(3,DeepWater)]
terrainInterpRule (DeepWater,_) = [(3,Water)]
terrainInterpRule (DeepForest,DeepForest) = []
terrainInterpRule (DeepForest,Forest) = [(3,DeepForest)]
terrainInterpRule (DeepForest,_) = [(5,Forest)]
terrainInterpRule (Forest,DeepForest) = []
terrainInterpRule (Forest,Forest) = []
terrainInterpRule (Forest,_) = [(1,Grass)]
terrainInterpRule (Water,Water) = [(20,Water),(1,Sand)]
terrainInterpRule (Water,DeepWater) = []
terrainInterpRule (Water,_) = [(1,Sand)]
terrainInterpRule (Sand,Desert) = [(1,Grass),(1,Forest)]
terrainInterpRule _ = []

-- |
-- A list of every TerrainPatch that might be created from the terrainFrequencies function.
--
baseTerrainPatches :: [TerrainPatch]
baseTerrainPatches = nub $ List.map snd $ concatMap terrainFrequencies [minBound..maxBound]

terrainInterpMap :: Map (TerrainPatch,TerrainPatch) [(Integer,TerrainPatch)]
terrainInterpMap = let terrain_patch_pairs = [(a,b) | a <- baseTerrainPatches, b <- baseTerrainPatches]
		       interps = List.map terrainInterpFn terrain_patch_pairs
		       in fromList (zip terrain_patch_pairs interps)

type TerrainGrid = Grid TerrainPatch

-- |
-- Generates a random terrain map.  The Biome indicates determines what TerrainPatches
-- are generated.  The second parameter is an Integer that indicates the smootheness of the
-- generated terrain.  Finally, a random Integer stream is needed to provide the random data 
-- to generate the terrain.
--
generateTerrain :: TerrainGenerationData -> [Integer] -> TerrainGrid
generateTerrain tg rands = flip (foldr placeTerrain) (tg_placements tg) $
    generateGrid (terrainFrequencies (tg_biome tg))
		 terrainInterpMap
		 (tg_smootheness tg)
		 rands

terrainPatchToASCII :: TerrainPatch -> Char
terrainPatchToASCII RockFace = '#'
terrainPatchToASCII Rubble = '*'
terrainPatchToASCII (Ore _) = '$'
terrainPatchToASCII RockyGround = ':'
terrainPatchToASCII Dirt = '.'
terrainPatchToASCII Grass = ','
terrainPatchToASCII Sand = '_'
terrainPatchToASCII Desert = '_'
terrainPatchToASCII Forest = 'f'
terrainPatchToASCII DeepForest = 'F'
terrainPatchToASCII Water = '~'
terrainPatchToASCII DeepWater = '~'
terrainPatchToASCII Ice = '^'
terrainPatchToASCII Glass = '_'
terrainPatchToASCII Lava = '^'
terrainPatchToASCII RecreantFactory = 'o'

exampleTerrainGenerator :: TerrainGenerationData
exampleTerrainGenerator = TerrainGenerationData
			  { tg_smootheness = 5,
			    tg_biome = ForestBiome,
			    tg_placements = [] }

generateExampleTerrain :: Integer -> TerrainGrid
generateExampleTerrain seed = generateTerrain exampleTerrainGenerator (randomIntegerStream seed)

prettyPrintTerrain :: ((Integer,Integer),(Integer,Integer)) -> TerrainGrid -> [String]
prettyPrintTerrain ((left_bound,right_bound),(top_bound,bottom_bound)) terrain_map =
    [[terrainPatchToASCII $ gridAt terrain_map (x,y) 
      | x <- [left_bound..right_bound]]
     | y <- [top_bound..bottom_bound]]
