{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Turns
    (dbPerformPlayerTurn)
    where

import Control.Monad.Maybe
import Control.Monad.Trans
import DB
import FactionData
import SpeciesData
import Plane
import Control.Monad
import Creature
import Data.Ratio
import Facing
import TerrainData
import Data.Maybe
import Behavior
import qualified Perception as P
import Position
import PlayerState
import Logging

dbPerformPlayerTurn :: Behavior -> CreatureRef -> DB ()
dbPerformPlayerTurn beh creature_ref =
    do logDB log_turns INFO $ "Beginning player action: " ++ show beh
       dbBehave beh creature_ref
       logDB log_turns INFO $ "Finishing AI turns."
       dbFinishPendingAITurns

dbFinishPendingAITurns :: DB ()
dbFinishPendingAITurns =
    do m_current_plane <- dbGetCurrentPlane
       case m_current_plane of
           Just p -> dbFinishPlanarAITurns p
           Nothing -> return ()

dbFinishPlanarAITurns :: PlaneRef -> DB ()
dbFinishPlanarAITurns plane_ref =
    do sweepDead plane_ref
       (all_creatures_on_plane :: [CreatureRef]) <- dbGetContents plane_ref
       any_players_left <- liftM (any (== Player)) $ mapM getCreatureFaction all_creatures_on_plane
       next_turn <- dbNextTurn $ map generalizeReference all_creatures_on_plane ++ [generalizeReference plane_ref]
       case next_turn of
           _ | not any_players_left ->
               do setPlayerState GameOver
                  return ()
           ref | ref =:= plane_ref ->
               do dbPerform1PlanarAITurn plane_ref
                  dbFinishPlanarAITurns plane_ref
           ref | Just creature_ref <- coerceReferenceTyped _creature ref ->
               do faction <- getCreatureFaction creature_ref
                  if (faction /= Player)
                      then do dbPerform1CreatureAITurn creature_ref
                              dbFinishPlanarAITurns plane_ref
                      else setPlayerState (PlayerCreatureTurn creature_ref NormalMode)
                  return ()
           _ -> error "dbFinishPlanarAITurns: impossible case"

planar_turn_frequency :: Integer
planar_turn_frequency = 100

monster_spawns :: [(TerrainPatch,Species)]
monster_spawns = [(RecreantFactory,Recreant), (Dirt,DustVortex)]

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref =
    do creature_locations <- dbGetContents plane_ref
       player_locations <- filterRO (liftM (== Player) . getCreatureFaction . child) creature_locations
       num_npcs <- liftM length $ filterRO (liftM (/= Player) . getCreatureFaction . child) creature_locations
       when (num_npcs < length player_locations * 2) $
           do (terrain_type,species) <- pickM monster_spawns
              _ <- spawnNPC terrain_type species plane_ref $ map parent $ player_locations
              return ()
       dbAdvanceTime plane_ref (1%planar_turn_frequency)

-- |
-- Spawn a non-player creature on the specified terrain type (or fail if not finding that terrain type)
-- and of the specified species, on the specified plane, near one of the specified positions
-- (presumably the list of positions of all player characters).
spawnNPC :: TerrainPatch -> Species -> PlaneRef -> [Position] -> DB Bool
spawnNPC terrain_type species plane_ref player_locations =
    do p <- pickM player_locations
       m_spawn_position <- pickRandomClearSite_withTimeout (Just 2) 7 0 0 p (== terrain_type) plane_ref
       case m_spawn_position of
           Nothing -> return False
           Just spawn_position ->
               do _ <- newCreature Pirates species (Standing plane_ref spawn_position Here)
                  return True

dbPerform1CreatureAITurn :: CreatureRef -> DB ()
dbPerform1CreatureAITurn creature_ref = liftM (const ()) $ atomic (flip dbBehave creature_ref) $
    P.runPerception creature_ref $ liftM (fromMaybe Vanish) $ runMaybeT $
        do player <- MaybeT $ liftM listToMaybe $ filterM (liftM (== Player) . P.getCreatureFaction . child) =<< P.visibleObjects (return . const True)
           (rand_x :: Integer) <- lift $ getRandomR (1,100)
           rand_face <- lift $ pickM [minBound..maxBound]
           (_,my_position) <- lift P.whereAmI
	   let face_to_player = faceAt my_position (parent player)
	   return $ case distanceBetweenChessboard my_position (parent player) of
               _ | rand_x < 5 -> Wait -- if AI gets stuck, this will make sure they waste time so the game doesn't hang
               _ | rand_x < 20 -> Step rand_face
	       1 -> Attack face_to_player
               -- x | x >= 10 -> Jump face_to_player  -- disable this until we can handle non-player teleporting sanely
	       _ -> Step face_to_player

