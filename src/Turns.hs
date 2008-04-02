{-# LANGUAGE PatternGuards #-}

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
    do all_creatures_on_plane <- liftM (map genericChild . filter isCreatureLocation) $ dbGetContents plane_ref
       next_turn <- dbNextTurn $ all_creatures_on_plane ++ [genericReference plane_ref, genericReference the_universe]
       case next_turn of
           ref | ref == genericReference the_universe -> 
	       do dbPerform1UniverseAITurn
	          dbFinishPlanarAITurns plane_ref
	   ref | ref == genericReference plane_ref -> 
	       do dbPerform1PlanarAITurn plane_ref
	          dbFinishPlanarAITurns plane_ref
	   ref | Just creature_ref <- toCreatureRef ref -> 
	       do faction <- dbGetCreatureFaction creature_ref
	          if (faction /= Player)
		      then do dbPerform1CreatureAITurn creature_ref
		              dbFinishPlanarAITurns plane_ref
		      else dbSetState (DBPlayerCreatureTurn creature_ref DBNotSpecial)
		  return ()
	   _ -> error "dbFinishPlanarAITurns: impossible case"

dbPerform1UniverseAITurn :: DB ()
dbPerform1UniverseAITurn = dbAdvanceTime (1%100) the_universe -- 1/100 is an arbitrary value smaller than most creature turns should take
                                                              -- this is the clock of the entire universe!

dbPerform1PlanarAITurn :: PlaneRef -> DB ()
dbPerform1PlanarAITurn plane_ref = 
    do creature_locations <- liftM (mapMaybe coerceLocation) $ dbGetContents plane_ref
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
dbPerform1CreatureAITurn = dbAdvanceTime (1%10)
