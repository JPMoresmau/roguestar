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
  | PhasePistol
      deriving (Eq,Ord,Show)
\end{code}      
