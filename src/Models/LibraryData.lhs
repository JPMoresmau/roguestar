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
  | PhasePistol
  | MachineArmLower
  | MachineArmUpper
  | ThinLimb
      deriving (Eq,Ord,Show)
\end{code}      
