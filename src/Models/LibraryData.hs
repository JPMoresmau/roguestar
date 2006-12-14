module Models.LibraryData
    (LibraryModel(..))
    where
    
data LibraryModel = QuestionMark
                  | AscensionClassStarship       -- spaceships
                  | Encephalon                   -- creatures
                  deriving (Eq,Ord)