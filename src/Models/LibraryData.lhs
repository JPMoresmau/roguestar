\section{Library Models}

\begin{code}
module Models.LibraryData
    (LibraryModel(..))
    where

import Models.Sky

data LibraryModel = 
    -- Terrain
    TerrainTile String
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
    -- Tools
  | PhasePistol
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
      deriving (Eq,Ord,Show)
\end{code}      
