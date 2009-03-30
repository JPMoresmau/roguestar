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
import RSAGL.LODCache
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
import Models.CyborgType4
import Models.EnergyThings
import Models.EnergySwords
import Models.Spheres
import Control.Exception

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
toModel Phaser = phaser
toModel PhaseRifle = phase_rifle
toModel GasSphere = gasSphere
toModel MetalSphere = metalSphere
toModel ChromaliteSphere = chromaliteSphere
toModel MachineArmLower = machine_arm_lower
toModel MachineArmUpper = machine_arm_upper
toModel CaduceatorArmLower = caduceator_arm_lower
toModel CaduceatorArmUpper = caduceator_arm_upper
toModel ReptilianLegLower = reptilian_leg_lower
toModel ReptilianLegUpper = reptilian_leg_upper
toModel ReptilianArmLower = reptilian_arm_lower
toModel ReptilianArmUpper = reptilian_arm_upper
toModel ThinLimb = thin_limb
toModel CyborgType4Dome = cyborg_type_4_dome
toModel CyborgType4Base = cyborg_type_4_base
toModel CyborgType4HyperspaceDisc = cyborg_type_4_hyperspace_disc
toModel CyborgType4HyperspaceRotor = cyborg_type_4_hyperspace_rotor
toModel CyborgType4HyperspaceStabilizer = cyborg_type_4_hyperspace_stabilizer
toModel (EnergyCylinder c) = energyCylinder c
toModel (EnergySword c n) = energySword c n

-- |
-- Sometimes we want to constrain the quality of some models.
--
forceQuality :: LibraryModel -> Quality -> Quality
forceQuality CyborgType4HyperspaceRotor = const Bad                               -- ambient box, LOD doesn't affect appearance
forceQuality (TerrainTile s) | not (s `elem` ["forest","deepforest"]) = min Poor  -- terrain just isn't that interesting
forceQuality _ = id

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
     Phaser,
     PhaseRifle,
     MachineArmLower,
     MachineArmUpper,
     CaduceatorArmLower,
     CaduceatorArmUpper,
     ReptilianLegLower,
     ReptilianLegUpper,
     ReptilianArmLower,
     ReptilianArmUpper,
     ThinLimb,
     CyborgType4Dome,
     CyborgType4Base,
     CyborgType4HyperspaceDisc,
     CyborgType4HyperspaceRotor,
     CyborgType4HyperspaceStabilizer]

-- |
-- A library of named models.  Models are generated on demand, but models
-- known in all_library_models are generated at worst quality when the library is first initialized.
--
data Library = Library 
    Bottleneck
    (MVar (Map LibraryModel (LODCache Quality BakedModel)))

-- |
-- Create a new library.  Only one Library should be needed per process instance, i.e. a singleton.
--
newLibrary :: IO Library
newLibrary = 
    do lib <- liftM2 Library simpleBottleneck (newMVar Map.empty)
       mapM_ (\x -> do hPutStrLn stderr ("newLibrary: preloading model: " ++ show x)
                       lookupModel lib x Bad) all_library_models
       return lib

-- |
-- Get a library model.  If the model is not available at the requested quality level, a
-- poorer model will be provided instead, and a background thread will launch to begin
-- generating the model at the requested level of detail.
--
-- If the model is not available at any level of quality, this may block until the model is completed.
--
lookupModel :: Library -> LibraryModel -> Quality -> IO IntermediateModel
lookupModel (Library bottleneck lib) lm q_ = bracket (takeMVar lib) (\lib_map -> tryPutMVar lib lib_map) $ \lib_map ->
    do let q = forceQuality lm q_
       let m_qo = Map.lookup lm lib_map
       case m_qo of
           Just qo -> liftM toIntermediateModel $ getLOD qo q
	   Nothing ->
	       do hPutStrLn stderr ("lookupModel: introducing model: " ++ show lm)
                  qo <- newLODCache bottleneck (\q' -> bakeModel $ buildIntermediateModel (qualityToVertices q') (toModel lm q')) [Bad,Poor,Good,Super]
                  putMVar lib $ insert lm qo lib_map
	          liftM toIntermediateModel $ getLOD qo q
