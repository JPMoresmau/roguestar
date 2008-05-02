\section{Library Models}

\begin{code}
module Models.LibraryData
    (LibraryModel(..))
    where

data LibraryModel = 
    TerrainTile String
  | QuestionMark
  | NullModel
  | Encephalon
  | Recreant
  | Androsynth
  | AscendantGlow
  | Caduceator
  | PhasePistol
  | MachineArmLower
  | MachineArmUpper
  | CaduceatorArmLower
  | CaduceatorArmUpper
  | ThinLimb
      deriving (Eq,Ord,Show)
\end{code}      
