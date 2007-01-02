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
toModel Recreant = recreant
toModel PhasePistol = phase_pistol

-- |
-- Automatically managing display lists, display the specified model at the specified quality.
--
displayLibraryModel :: IORef RoguestarGlobals -> LibraryModel -> Quality -> IO ()
displayLibraryModel globals_ref model q =
    do model_lookup_table <- liftM global_library_models $ readIORef globals_ref
       let display_list = Map.lookup (model,q) model_lookup_table
       if isJust display_list       -- if we already have a display list for this model, use it, otherwise make one
         then callList $ fromJust display_list
         else do new_display_list <- defineNewList CompileAndExecute (toOpenGL $ toModel model q)
                 modifyIORef globals_ref (\g -> g {global_library_models = insert (model,q) new_display_list (global_library_models g)}) 