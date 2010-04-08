module Models.LibraryData
    (LibraryModel(..),EnergyColor(..))
    where

import Models.Sky
import qualified Data.ByteString.Char8 as B

data EnergyColor = Blue | Yellow | Red | Green deriving (Eq,Ord,Show)

data LibraryModel = 
    -- Terrain
    TerrainTile B.ByteString
    -- Astronomical Phenomena
  | SkySphere SkyInfo
  | SunDisc SunInfo
    -- The Null Model
  | NullModel
    -- The Question Mark Object
  | QuestionMark
    -- Creature Bodies
  | Encephalon
  | Recreant
  | Androsynth
  | AscendantGlow
  | Caduceator
  | Reptilian
    -- Energy Things
  | EnergyCylinder EnergyColor
    -- Tools
  | PhasePistol
  | Phaser
  | PhaseRifle
  | EnergySword EnergyColor Integer
  | GasSphere
  | MetalSphere
  | ChromaliteSphere
    -- Arms and Legs
  | MachineArmLower
  | MachineArmUpper
  | CaduceatorArmLower
  | CaduceatorArmUpper
  | ReptilianLegUpper
  | ReptilianLegLower
  | ReptilianArmUpper
  | ReptilianArmLower
  | ThinLimb
    -- Space Ship Parts
  | CyborgType4Dome
  | CyborgType4Base
  | CyborgType4HyperspaceDisc
  | CyborgType4HyperspaceRotor
  | CyborgType4HyperspaceStabilizer
    -- Buildings
  | Monolith
  | Portal
      deriving (Eq,Ord,Show)
