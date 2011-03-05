module Models.LibraryData
    (LibraryModel(..),
     SimpleModel(..),
     EnergyColor(..),
     EnergyThing(..),
     LibraryModelSource(..))
    where

import Models.Sky
import qualified Data.ByteString.Char8 as B

data EnergyColor = Blue | Yellow | Red | Green
         deriving (Eq,Ord,Show,Enum,Bounded)

data SimpleModel =
    LeafyBlob
  | TreeBranch
  | QuestionMark
    -- Creature Bodies
  | Encephalon
  | Recreant
  | Androsynth
  | AscendantGlow
  | DustPuff
  | Caduceator
  | Reptilian
  | Hellion
    -- Tools
  | PhasePistol
  | Phaser
  | PhaseRifle
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
  | HellionAppendage
  | ThinLimb
    -- Other bodyparts
  | HellionEye
    -- Space Ship Parts
  | CyborgType4Dome
  | CyborgType4Base
  | CyborgType4HyperspaceDisc
  | CyborgType4HyperspaceRotor
  | CyborgType4HyperspaceStabilizer
    -- Buildings
  | Monolith
  | PlanetaryAnchorCore
  | PlanetaryAnchorFlange
  | Portal
      deriving (Eq,Ord,Show,Enum,Bounded)

data EnergyThing =
    EnergySword
  | EnergyCylinder
      deriving (Eq,Ord,Show,Enum,Bounded)

data LibraryModel =
    -- Terrain
    TerrainTile B.ByteString
    -- Astronomical Phenomena
  | SkySphere SkyInfo
  | SunDisc SunInfo
    -- The Null Model
  | NullModel
    -- SimpleModels (zero-parameter models)
  | SimpleModel SimpleModel
    -- Energy things
  | EnergyThing EnergyThing EnergyColor
      deriving (Eq,Ord,Show)

-- | Things that are also LibraryModels.
class LibraryModelSource a where
    toLibraryModel :: a -> LibraryModel

instance LibraryModelSource LibraryModel where
    toLibraryModel = id

instance LibraryModelSource SimpleModel where
    toLibraryModel = SimpleModel

