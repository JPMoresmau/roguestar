{-# LANGUAGE OverloadedStrings #-}

module Models.Library
    (Library,
     newLibrary,
     lookupModel)
    where

import Quality
import Data.Map as Map
import RSAGL.Modeling hiding (model)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc
import PrioritySync.PrioritySync

import Models.LibraryData

import Models.Terrain
import Models.QuestionMark
import Models.Tree
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
import Models.Monolith
import Models.Stargate

-- |
-- Get the modeling data for a named library model.
--
toModel :: LibraryModel -> Quality -> Modeling ()
toModel (TerrainTile s) = terrainTile s
toModel LeafyBlob = const $ leafy_blob
toModel TreeBranch = const $ tree_branch
toModel (SkySphere sky_info) = const $ makeSky sky_info
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
toModel Monolith = monolith
toModel Portal = portal

-- |
-- Models that should be displayed at lower quality.
--
downgraded_models :: [LibraryModel]
downgraded_models = [LeafyBlob,TreeBranch,
                     GasSphere,MetalSphere,ChromaliteSphere,MachineArmLower,
                     MachineArmUpper,CaduceatorArmLower,CaduceatorArmUpper,
                     ReptilianLegLower,ReptilianLegUpper,ReptilianArmLower,
                     ReptilianArmUpper, ThinLimb]

-- |
-- Sometimes we want to constrain the quality of some models.
--
forceQuality :: LibraryModel -> Quality -> Quality
-- ambient box, LOD doesn't affect appearance
forceQuality CyborgType4HyperspaceRotor = const Bad
-- terrain just isn't that interesting
forceQuality (TerrainTile _) = min Poor
forceQuality x | x `elem` downgraded_models = \q -> case q of
    Bad -> Bad
    Poor -> Bad
    Good -> Poor
    Super -> Poor
forceQuality _ = id

-- |
-- Models to build at start time.
--
essential_library_models :: [LibraryModel]
essential_library_models =
    Prelude.map TerrainTile known_terrain_types ++
    [Encephalon,
     Recreant,
     Androsynth,
     Caduceator,
     Reptilian,
     AscendantGlow]

modelPriority :: Quality -> LibraryModel -> Integer
modelPriority Bad model | model `elem` essential_library_models = 0
modelPriority q (SkySphere {}) = 10 * qualityPriority q
modelPriority q _ = qualityPriority q

qualityPriority :: Quality -> Integer
qualityPriority Bad = 1
qualityPriority Poor = 2
qualityPriority Good = 4
qualityPriority Super = 8

-- |
-- A library of named models.  Models are generated on demand, but models
-- known in all_library_models are generated at worst quality when the library
-- is first initialized.
--
data Library = Library {
    library_cache :: TVar (Map.Map (Quality,LibraryModel)
                                   (TVar (Maybe BakedModel))),
    library_wrapper :: Quality -> LibraryModel -> IO () -> IO () }

-- |
-- Create a new library.
-- Only one Library should be needed per process instance.
--
newLibrary :: IO Library
newLibrary =
    do task_pool <- newTaskPool fast_queue_configuration
                                (max 1 $ numCapabilities - 2)
                                ()
       startQueue task_pool
       cache <- newTVarIO Map.empty
       let lib = Library cache $ \q m actionIO ->
                     do _ <- dispatch (schedule task_pool $ modelPriority q m)
                                      actionIO
                        return ()
       mapM_ (\x -> lookupModel lib x Bad) essential_library_models
       return lib

-- |
-- Get a library model.  If the model is not available at the requested quality level, a
-- poorer model will be provided instead, and a background thread will launch to begin
-- generating the model at the requested level of detail.
--
lookupModel :: Library -> LibraryModel -> Quality -> IO IntermediateModel
lookupModel lib model q_ = (id =<<) $ atomically $
    do let q = forceQuality model q_
       cache <- readTVar $ library_cache lib
       let m_var = Map.lookup (q,model) cache
       case m_var of
           Just var ->
               do m_result <- liftM (fmap toIntermediateModel) $ readTVar var
                  case m_result of
                      Just result -> return $ return result
                      Nothing | model == NullModel ->
                          return $ atomically $ liftM toIntermediateModel $
                              maybe retry return =<< readTVar var
                      Nothing | q == Bad -> return $
                          lookupModel lib NullModel Bad
                      Nothing -> return $
                          lookupModel lib model (pred q)
           Nothing ->
               do target <- newTVar Nothing
                  writeTVar (library_cache lib) $
                      Map.insert (q,model) target cache
                  return $
                      do library_wrapper lib q model $
                             bakeAndStore target model q
                         lookupModel lib model q


bakeAndStore :: TVar (Maybe BakedModel) -> LibraryModel -> Quality -> IO ()
bakeAndStore target model q =
    do result <- bakeModel $ buildIntermediateModel (qualityToVertices q)
                                                    (toModel model q)
       atomically $ writeTVar target $ Just result

