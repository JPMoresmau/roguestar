module TerrainData
    where

import PeriodicTable

-- |
-- Most automatically generated surface maps belong to a Biome, representing the kind of terrain
-- and plant life that dwells in terrain generated for the map.
--
data Biome = RockBiome
           | IcyRockBiome
           | GrasslandBiome
	   | ForestBiome
           | TundraBiome
           | DeasertBiome
           | OceanBiome
           | MountainsBiome
	     deriving (Read,Show,Eq,Ord,Enum,Bounded)

-- |
-- All static terrain elements are members of TerrainMap
--
-- The only difference between "Deasert" and "Sand" is that where
-- "Deasert" and "Water" touch, the map generator will produce
-- patches of plantlife (for oasis and shoreline effect).
--
data TerrainPatch = RockFace
                  | Rubble
                  | Ore Element
                  | RockyGround
                  | Dirt
                  | Grass
                  | Sand
                  | Deasert
                  | Forest
                  | DeepForest
                  | Water
                  | DeepWater
                  | Ice
                  | DungeonEntrance Integer
                  | DungeonExit
                    deriving (Read,Show,Eq,Ord)

terrainFrequencies :: Biome -> [TerrainPatch]
terrainFrequencies RockBiome = [(1,RockFace),(1,Rubble),(3,RockyGround),(1,Sand)]
terrainFrequencies IcyRockBiome = [(1,RockFace),(2,Rubble),(3,RockyGround),(6,Ice)]
terrainFrequencies GrasslandBiome = [(1,RockFace),(1,RockyGround),(1,Dirt),(2,Sand),(1,Forest),(1,Water),(10,Grass)]
terrainFrequencies ForestBiome = [(1,RockFace),(1,RockyGround),(1,Dirt),(5,Water),(3,Grass),(5,Forest),(5,DeepForest)]
terrainFrequencies TundraBiome = [(1,RockFace),(3,RockyGround),(1,Sand),(1,Water),(1,Grass),(8,Ice)]
terrainFrequencies DeasertBiome = [(1,RockFace),(3,RockyGround),(1,Grass),(1,Water),(11,Deasert)]
terrainFrequencies OceanBiome = [(1,RockyGround),(3,Sand),(1,Grass),(1,Forest),(7,Water),(20,DeepWater)]
terrainFrequencies MountainBiome = [(6,RockFace),(3,RockyGround),(1,Rubble),(1,Sand),(1,Grass),(1,Forest),(1,Water)]

terrainInterpFn :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpFn (a,b) = let explicit_map = [(1,a),(1,b)] ++ (terrainInterp (a,b)) ++ (terrainInterp (b,a))

terrainInterpRule :: (TerrainPatch,TerrainPatch) -> [(Integer,TerrainPatch)]
terrainInterpRule (RockFace,RockFace) = []
terrainInterpRule (RockFace,RockyGround) = [(3,RockFace),(1,Rubble),(3,RockyGround)]
terrainInterpRule (RockFace,x) = [(3,RockFace),(2,Rubble),(1,RockyGround),(1,Sand),(7,x)]
terrainInterpRule (Rubble,x) = [(1,Rubble),(2,Sand),(2,Dirt),(5,x)]
terrainInterpRule (DeepWater,DeepWater) = []
terrainInterpRule (DeepWater,_) = [(3,Water)]
terrainInterpRule (DeepForest,DeepForest) = []
terrainInterpRule (DeepForest,_) = [(5,Forest)]
terrainInterpRule (Water,Water) = []
terrainInterpRule (Water,DeepWater) = []
terrainInterpRule (Water,_) = [(1,Sand)]
terrainInterpRule (Sand,Deasert) = [(1,Grass),(1,Forest)]

-- |
-- A list of every TerrainPatch that might be created from the terrainFrequencies function.
--
baseTerrainPatches :: [TerrainPatch]
baseTerrainPatches = nub $ map snd $ foldr (++) [] $ map terrainFrequencies [maxBound..minBound]

terrainInterpMap :: Map (TerrainPatch,TerrainPatch) [(Integer,TerrainPatch)]
terrainInterpMap = let terrain_patch_pairs = [(a,b) | a <- baseTerrainPatches, b <- baseTerrainPatches]
		       interps = map terrainInterpFn terrain_patch_pairs
		       in fromList (zip terrain_patch_pairs interps)