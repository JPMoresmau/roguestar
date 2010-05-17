{-# LANGUAGE OverloadedStrings #-}

module Models.Library
    (Library,
     newLibrary,
     lookupModel)
    where

import Quality
import Data.Maybe
import Data.Map as Map
import RSAGL.Modeling hiding (model)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc
import PrioritySync.PrioritySync
import System.IO
import Control.Concurrent.MVar

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
toModel (SimpleModel LeafyBlob) = const $ leafy_blob
toModel (SimpleModel TreeBranch) = const $ tree_branch
toModel (SkySphere sky_info) = const $ makeSky sky_info
toModel (SunDisc sun_info) = const $ makeSun sun_info
toModel (SimpleModel QuestionMark) = const $ question_mark
toModel NullModel = const $ return ()
toModel (SimpleModel Encephalon) = encephalon
toModel (SimpleModel Recreant) = recreant
toModel (SimpleModel Androsynth) = androsynth
toModel (SimpleModel Caduceator) = caduceator
toModel (SimpleModel Reptilian) = reptilian
toModel (SimpleModel AscendantGlow) = ascendant_glow
toModel (SimpleModel PhasePistol) = phase_pistol
toModel (SimpleModel Phaser) = phaser
toModel (SimpleModel PhaseRifle) = phase_rifle
toModel (SimpleModel GasSphere) = gasSphere
toModel (SimpleModel MetalSphere) = metalSphere
toModel (SimpleModel ChromaliteSphere) = chromaliteSphere
toModel (SimpleModel MachineArmLower) = machine_arm_lower
toModel (SimpleModel MachineArmUpper) = machine_arm_upper
toModel (SimpleModel CaduceatorArmLower) = caduceator_arm_lower
toModel (SimpleModel CaduceatorArmUpper) = caduceator_arm_upper
toModel (SimpleModel ReptilianLegLower) = reptilian_leg_lower
toModel (SimpleModel ReptilianLegUpper) = reptilian_leg_upper
toModel (SimpleModel ReptilianArmLower) = reptilian_arm_lower
toModel (SimpleModel ReptilianArmUpper) = reptilian_arm_upper
toModel (SimpleModel ThinLimb) = thin_limb
toModel (SimpleModel CyborgType4Dome) = cyborg_type_4_dome
toModel (SimpleModel CyborgType4Base) = cyborg_type_4_base
toModel (SimpleModel CyborgType4HyperspaceDisc) = cyborg_type_4_hyperspace_disc
toModel (SimpleModel CyborgType4HyperspaceRotor) =
        cyborg_type_4_hyperspace_rotor
toModel (SimpleModel CyborgType4HyperspaceStabilizer) =
        cyborg_type_4_hyperspace_stabilizer
toModel (EnergyThing EnergyCylinder c) = energyCylinder c
toModel (EnergyThing EnergySword c) = energySword c 3
toModel (SimpleModel Monolith) = monolith
toModel (SimpleModel Portal) = portal

-- |
-- Models that should be displayed at lower quality.
--
downgraded_models :: [LibraryModel]
downgraded_models =
    Prelude.map toLibraryModel
        [LeafyBlob,TreeBranch,
         GasSphere,MetalSphere,ChromaliteSphere,MachineArmLower,
         MachineArmUpper,CaduceatorArmLower,CaduceatorArmUpper,
         ReptilianLegLower,ReptilianLegUpper,ReptilianArmLower,
         ReptilianArmUpper, ThinLimb]

-- |
-- Sometimes we want to constrain the quality of some models.
--
forceQuality :: LibraryModel -> Quality -> Quality
-- ambient box, LOD doesn't affect appearance
forceQuality (SimpleModel CyborgType4HyperspaceRotor) = const Bad
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
    Prelude.map SimpleModel [minBound..maxBound] ++
    [EnergyThing x y | x <- [minBound..maxBound],
                       y <- [minBound..maxBound]]

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
       loading_stdout_mvar <- newMVar ()
       let lib = Library cache $ \q m actionIO ->
                     do _ <- dispatch (schedule task_pool $ modelPriority q m)
                                      actionIO
                        return ()
       forM_ essential_library_models $ \x -> lookupModel lib x Bad
       let waitFor x = atomically $
                  do b <- isLoaded lib x
                     when (not b) retry
       forM_ essential_library_models $ \x -> forkIO $
           do waitFor x
              modifyMVar_ loading_stdout_mvar $ const $
                  do hPutStrLn stderr $ "Pregenerated model: " ++ show x
                     return ()
       forM_ essential_library_models $ \x -> waitFor x
       return lib

-- |
-- Get a library model.  If the model is not available at the requested quality
-- level, a poorer model will be provided instead, and a background thread will
-- launch to begin generating the model at the requested level of detail.
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

isLoaded :: Library -> LibraryModel -> STM Bool
isLoaded lib model =
    do cache <- readTVar $ library_cache lib
       let m_var = Map.lookup (Bad,model) cache
       maybe (return False) (liftM isJust . readTVar) m_var

bakeAndStore :: TVar (Maybe BakedModel) -> LibraryModel -> Quality -> IO ()
bakeAndStore target model q =
    do result <- bakeModel $ buildIntermediateModel (qualityToVertices q)
                                                    (toModel model q)
       atomically $ writeTVar target $ Just result

