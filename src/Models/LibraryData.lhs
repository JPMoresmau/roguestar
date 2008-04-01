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
  | PhasePistol
  | MachineArmLower
  | MachineArmUpper
      deriving (Eq,Ord,Show)
\end{code}      
