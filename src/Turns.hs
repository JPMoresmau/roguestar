{-# LANGUAGE PatternGuards, PatternSignatures #-}

module Turns
    (dbPerformPlayerTurn)
    where

import DB
import DBData
import FactionData
import Races
import Plane
import Control.Monad
import Creature
import Data.Ratio
import Facing
import Dice
import TerrainData
import Data.Maybe
import Behavior
import Perception

dbPerformPlayerTurn :: Behavior -> CreatureRef -> DB ()
dbPerformPlayerTurn beh creature_ref =
    do dbBehave beh creature_ref
       dbFinishPendingAITurns

dbFinishPendingAITurns :: DB ()
dbFinishPendingAITurns =
    do m_current_plane <- dbGetCurrentPlane
       case m_current_plane of
           Just p -> dbFinishPlanarAITurns p
	   Nothing -> return ()

dbFinishPlanarAITurns :: PlaneRef -> DB ()
dbFinishPlanarAITurns plane_ref =
    do (all_creatures_on_plane :: [CreatureRef]) <- dbGetContents plane_ref
       next_turn <- dbNextTurn $ map generalizeReference all_creatures_on_plane ++ [generalizeReference plane_ref, generalizeReference the_universe]
       case next_turn of
           ref | ref =:= the_universe -> 
	       do dbPerform1UniverseAITurn
	          dbFinishPlanarAITurns plane_ref
	   ref | ref =:= plane_ref -> 
	       do dbPerform1PlanarAITurn plane_ref
	          dbFinishPlanarAITurns plane_ref
	   ref | Just creature_ref <- coerceReferenceTyped _creature ref -> 
	       do faction <- dbGetCreatureFaction creature_ref
	          if (faction /= Player)
		      then do dbPerform1CreatureAITurn creature_ref
		              dbFinishPlanarAITurns plane_ref
		      else dbSetState (DBPlayerCreatureTurn creature_ref DBNormal)
		  return ()
	   _ -> error "dbFinishPlanarAITurns: impossible case"

dbPerform1UniverseAITurn :: DB ()
dbPerform1UniverseAITurn = dbAdvanceTime (1%100) the_universe 

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref = 
    do creature_locations <- dbGetContents plane_ref
       player_locations <- filterRO (liftM (== Player) . dbGetCreatureFaction . entity) creature_locations
       native_locations <- filterRO (liftM (/= Player) . dbGetCreatureFaction . entity) creature_locations
       when (length native_locations < length player_locations * 2) $
           do p <- roll $ map location player_locations
	      spawn_position <- pickRandomClearSite 5 0 0 p (== RecreantFactory) plane_ref
	      dbNewCreature Pirates recreant (Standing plane_ref spawn_position Here)
	      return ()
       t <- roll [1%100,1%50,1%25,1%10,1%5,1%4,1%3] -- limit player's influence of when creatures spawn
       dbAdvanceTime t plane_ref

dbPerform1CreatureAITurn :: CreatureRef -> DB ()
dbPerform1CreatureAITurn creature_ref = atomic $ liftM (flip dbBehave creature_ref) $ runPerception creature_ref $
    do m_player <- liftM listToMaybe $ filterM (liftM (== Player) . creatureFaction . entity) =<< visibleObjects 
       maybe (return Wait)
           (\player -> liftM (Step . flip faceAt (location player)) myPosition)
	   m_player

