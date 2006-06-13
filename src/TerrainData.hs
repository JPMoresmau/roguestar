module TerrainData
    where

import PeriodicTable

data Biome = RockBiome
           | IcyRockBiome
           | GrasslandBiome
           | TundraBiome
           | DeasertBiome
           | OceanBiome
           | MountainsBiome

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
                    deriving (Read,Show)

data GeneratedTerrainGrid = CompletelyRandomGrid Integer Biome
                          | InterpolatedGrid Integer GeneratedTerrainGrid
                          | OreReplacementGrid Integer [(Integer,Element)] GeneratedTerrainGrid
                          | ReplacementGrid (Map (Integer,Integer) TerrainPatch)

terrainFrequencies :: Biome -> [TerrainPatch]
terrainFrequencies RockBiome = [RockFace,Rubble,RockyGround,RockyGround,RockyGround,Sand]
terrainFrequencies IcyRockBiome = [RockFace,Rubble,RockyGround,RockyGround,RockyGround,Ice,Ice,Ice]
terrainFrequencies GrasslandBiome = [RockFace,RockyGround,Dirt,Sand,Sand,Forest,Water,Grass,Grass,Grass,Grass,Grass,Grass,Grass]
terrainFrequencies TundraBiome = [RockFace,RockyGround,RockyGround,RockyGround,Sand,Water,Grass,Ice,Ice,Ice,Ice,Ice,Ice,Ice,Ice]
terrainFrequencies DeasertBiome = [RockFace,RockyGround,RockyGround,RockyGround,Grass,Water,Sand,Sand,Sand,Sand,Sand,Sand,Sand,Sand,Sand,Sand,Sand]
terrainFrequencies OceanBiome = [RockyGround,Sand,Sand,Sand,Grass,Forest,Water,Water,Water,Water,Water,Water,Water,DeepWater,DeepWater,DeepWater,DeepWater,DeepWater,DeepWater,DeepWater]
terrainFrequencies MountainBiome = [RockFace,RockFace,RockFace,RockFace,RockFace,RockFace,RockyGround,RockyGround,RockyGround,Rubble,Sand,Grass,Forest,Water]