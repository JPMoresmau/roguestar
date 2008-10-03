\section{Library Models}

\begin{code}
module Models.LibraryData
    (LibraryModel(..))
    where

import Models.Sky

data LibraryModel = 
    TerrainTile String
  | SkySphere SkyInfo
  | SunDisc SunInfo
  | QuestionMark
  | NullModel
  | Encephalon
  | Recreant
  | Androsynth
  | AscendantGlow
  | Caduceator
  | Reptilian
  | PhasePistol
  | MachineArmLower
  | MachineArmUpper
  | CaduceatorArmLower
  | CaduceatorArmUpper
  | ReptilianLegUpper
  | ReptilianLegLower
  | ReptilianArmUpper
  | ReptilianArmLower
  | ThinLimb
      deriving (Eq,Ord,Show)
\end{code}      
