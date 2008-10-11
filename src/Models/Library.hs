module Models.Library
    (Library,
     newLibrary,
     lookupModel)
    where
    
import Quality
import Models.LibraryData
import Models.Terrain
import Models.QuestionMark
import Data.Map as Map
import RSAGL.Model
import RSAGL.QualityControl
import Control.Concurrent
import RSAGL.Bottleneck
import Control.Monad
import System.IO
import Models.Encephalon
import Models.Recreant
import Models.Androsynth
import Models.Ascendant
import Models.Caduceator
import Models.Reptilian
import Models.PhaseWeapons
import Models.MachineParts
import Models.Sky

-- |
-- Get the modeling data for a named library model.
--
toModel :: LibraryModel -> Quality -> Modeling ()
toModel (TerrainTile s) = terrainTile s
toModel (SkySphere sky_info) = \q -> case q of
    Bad -> toModel NullModel Bad
    _ ->   makeSky sky_info
toModel (SunDisc sun_info) = const $ makeSun sun_info
toModel QuestionMark = const $ question_mark
toModel NullModel = const $ return ()
toModel Encephalon = encephalon
toModel Recreant = recreant
toModel Androsynth = androsynth
toModel Caduceator = caduceator
toModel Reptilian = reptilian
toModel AscendantGlow = ascendant_glow
toModel PhasePistol = phase_pistol
toModel MachineArmLower = machine_arm_lower
toModel MachineArmUpper = machine_arm_upper
toModel CaduceatorArmLower = caduceator_arm_lower
toModel CaduceatorArmUpper = caduceator_arm_upper
toModel ReptilianLegLower = reptilian_leg_lower
toModel ReptilianLegUpper = reptilian_leg_upper
toModel ReptilianArmLower = reptilian_arm_lower
toModel ReptilianArmUpper = reptilian_arm_upper
toModel ThinLimb = thin_limb

-- |
-- All known library models.  Some models can't be known at compile time, and these aren't listed here.
-- For example, sky sphere models are parameterized on the configuration of the local planet-sun system.
--
all_library_models :: [LibraryModel]
all_library_models =
    Prelude.map TerrainTile known_terrain_types ++
    [QuestionMark, NullModel,
     Encephalon,
     Recreant,
     Androsynth,
     Caduceator,
     Reptilian,
     AscendantGlow,
     PhasePistol,
     MachineArmLower,
     MachineArmUpper,
     CaduceatorArmLower,
     CaduceatorArmUpper,
     ReptilianLegLower,
     ReptilianLegUpper,
     ReptilianArmLower,
     ReptilianArmUpper,
     ThinLimb]

-- |
-- A library of named models.  Models are generated on demand, but models
-- known in all_library_models are generated at worst quality when the library is first initialized.
--
data Library = Library 
    Bottleneck
    (MVar (Map LibraryModel (QualityCache Quality IntermediateModel)))

-- |
-- Create a new library.  Only one Library should be needed per process instance, i.e. a singleton.
--
newLibrary :: IO Library
newLibrary = 
    do lib <- liftM2 Library newBottleneck (newMVar Map.empty)
       mapM_ (\x -> do hPutStrLn stderr ("Preloading model: " ++ show x)
                       lookupModel lib x Poor) all_library_models
       return lib

-- |
-- Get a library model.  If the model is not available at the requested quality level, a
-- poorer model will be provided instead, and a background thread will launch to begin
-- generated the model at the requested level of detail.
--
-- If the model is not available at any level of quality, this may block until the model is completed.
--
lookupModel :: Library -> LibraryModel -> Quality -> IO IntermediateModel
lookupModel (Library bottleneck lib) lm q =
    do lib_map <- takeMVar lib
       m_qo <- return $ Map.lookup lm lib_map
       case m_qo of
           Just qo ->
	       do putMVar lib lib_map
	          getQuality qo q
	   Nothing ->
	       do hPutStrLn stderr ("Introducing model: " ++ show lm)
	          qo <- newQuality bottleneck parIntermediateModel (\q' -> toIntermediateModel (qualityToVertices q') (toModel lm q')) [Bad,Poor,Good,Super]
                  putMVar lib $ insert lm qo lib_map
	          getQuality qo q
