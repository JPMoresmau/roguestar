{-# LANGUAGE PatternGuards #-}

module Creature 
    (dbGenerateInitialPlayerCreature,
     dbNewCreature,
     dbTurnCreature,
     dbStepCreature,
     Roll(..),
     dbRollCreatureScore,
     dbGetCreatureFaction,
     dbRollInjury,
     dbInjureCreature,
     dbGetDead,
     dbDeleteCreature,
     dbSweepDead)
    where

import Data.Maybe
import CreatureData
import DB
import SpeciesData
import Species
import DBData
import FactionData
import Facing
import Control.Monad.Error
import Dice
import Tool

-- |
-- Generates a new Creature from the specified species.
--
dbGenerateCreature :: Faction -> Species -> DB Creature
dbGenerateCreature faction species = 
    do (stats,attribs,name) <- generateCreatureData species
       random_id <- dbNextRandomInteger
       return (Creature { creature_stats=stats,
			  creature_attribs=attribs,
			  creature_species_name=name,
			  creature_random_id=random_id,
			  creature_damage=0,
			  creature_faction=faction})

-- |
-- During DBRaceSelectionState, generates a new Creature for the player character and sets it into the 
-- database's DBClassSelectionState.
--
dbGenerateInitialPlayerCreature :: Species -> DB ()
dbGenerateInitialPlayerCreature species = 
    do newc <- dbGenerateCreature Player species
       dbSetStartingRace species
       dbSetState (DBClassSelectionState newc)

-- |
-- Generates a new Creature from the specified Species and adds it to the database.
--
dbNewCreature :: (CreatureLocation l) => Faction -> Species -> l -> DB CreatureRef
dbNewCreature faction species loc = 
    do creature <- dbGenerateCreature faction species
       dbAddCreature creature loc

data Roll = Roll { 
    ideal_score :: Integer,
    other_situation_bonus :: Integer,
    actual_roll :: Integer }

dbRollCreatureScore :: (DBReadable db) => Score -> Integer -> CreatureRef -> db Roll
dbRollCreatureScore score bonus creature_ref =
    do ideal <- liftM ((+ bonus) . creatureScore score) $ dbGetCreature creature_ref
       actual <- roll [0..ideal]
       return $ Roll ideal bonus actual

-- |
-- Causes the creature to walk in the specified facing direction.
--
dbWalkCreature :: Facing -> (Integer,Integer) -> CreatureRef -> DB ()
dbWalkCreature face (x',y') creature_ref =
    do flip dbMove creature_ref $ \l -> return $ fromMaybe l $
          do p <- liftM location $ toPlanarLocation l
             Position (x,y) <- liftM location $ toPositionLocation l
             let standing = Standing { standing_plane = p,
                                       standing_position = Position (x+x',y+y'),
                                       standing_facing = face } 
	     return $ genericLocationP $ standCreature standing l
       return ()

dbStepCreature :: Facing -> CreatureRef -> DB ()
dbStepCreature face = dbWalkCreature face (facingToRelative face)

dbTurnCreature :: Facing -> CreatureRef -> DB ()
dbTurnCreature face = dbWalkCreature face (0,0)

dbGetCreatureFaction :: (DBReadable db) => CreatureRef -> db Faction
dbGetCreatureFaction = liftM creature_faction . dbGetCreature

dbRollInjury :: (DBReadable db) => CreatureRef -> Integer -> db Integer
dbRollInjury creature_ref damage_roll = 
    do damage_reduction <- liftM actual_roll $ dbRollCreatureScore DamageReduction 0 creature_ref
       return $ max 0 $ damage_roll - damage_reduction
       
dbInjureCreature :: Integer -> CreatureRef -> DB ()
dbInjureCreature x = dbModCreature $ \c -> c { creature_damage = creature_damage c + x }

dbGetDead :: (DBReadable db) => Reference a -> db [CreatureRef]
dbGetDead parent_ref =
    do critters <- liftM (mapMaybe $ toCreatureRef . entity) $ dbGetContents parent_ref
       filterRO (liftM (\c -> creatureScore HitPoints c <= 0) . dbGetCreature) critters

dbDeleteCreature :: CreatureRef -> DB ()
dbDeleteCreature = dbUnsafeDeleteObject $ \l ->
    do m_dropped_loc <- maybe (return Nothing) (liftM Just . dbDropTool) $ toToolLocation l
       return $ case () of
           () | Just dropped_loc <- m_dropped_loc -> genericLocation dropped_loc
	   () | otherwise -> error "dbKillCreature: no case for this type of entity"

dbSweepDead :: Reference a -> DB ()
dbSweepDead ref =
    do worst_to_best_critters <- sortByRO (liftM ideal_score . dbRollCreatureScore HitPoints 0) =<< dbGetDead ref
       flip mapM_ worst_to_best_critters $ \creature_ref ->
           do dbPushSnapshot (DBKilledEvent creature_ref)
	      dbDeleteCreature creature_ref
