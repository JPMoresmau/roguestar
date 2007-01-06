module Models.Library
    (displayLibraryModel)
    where
    
import Quality
import Models.QuestionMark
import Models.AscensionClassStarship
import Models.Encephalon
import Models.Recreant
import Models.PhaseWeapons
import Model
import Models.LibraryData
import Globals
import Data.IORef
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL.GL.DisplayLists
import Data.Map as Map

toModel :: LibraryModel -> Quality -> Model
toModel QuestionMark = question_mark
toModel AscensionClassStarship = ascension_class_starship
toModel Encephalon = encephalon
toModel (Arm Encephalon) = encephalon_arm
toModel (Shoulder Encephalon) = encephalon_shoulder
toModel Recreant = recreant
toModel PhasePistol = phase_pistol
toModel (Arm arm) = error $ "Models.Library.toModel: unknown Arm: " ++ show arm
toModel (Shoulder shoulder) = error $ "Models.Library.toModel: unknown Shoulder" ++ show shoulder

-- |
-- Implements tweaking of quality for some objects.  Arm and Shoulder pieces, for example,
-- are usually small and unimportant, and therefore rendered at lower quality.
-- We can also control this at the model level, but doing it at the library level ensures
-- that we don't keep multiple copies of models at the same quality.
-- At this time nothing is forced to a higher quality, only lower.
--
qualityOverride :: LibraryModel -> Quality -> Quality
qualityOverride (Arm _) = reduced2
qualityOverride (Shoulder _) = reduced2
qualityOverride PhasePistol = reduced
qualityOverride _ = id

-- |
-- Automatically managing display lists, display the specified model at the specified quality.
--
displayLibraryModel :: IORef RoguestarGlobals -> LibraryModel -> Quality -> IO ()
displayLibraryModel globals_ref model q = displayLibraryModel_ globals_ref model $ qualityOverride model q

displayLibraryModel_ :: IORef RoguestarGlobals -> LibraryModel -> Quality -> IO ()
displayLibraryModel_ globals_ref model q =
    do model_lookup_table <- liftM global_library_models $ readIORef globals_ref
       let display_list = Map.lookup (model,q) model_lookup_table
       maybe newList callList display_list
       where newList = do new_display_list <- defineNewList CompileAndExecute (toOpenGL $ toModel model q)
                          modifyIORef globals_ref (\g -> g {global_library_models = insert (model,q) new_display_list (global_library_models g)}) 