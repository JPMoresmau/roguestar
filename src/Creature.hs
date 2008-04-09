
module Creature 
    (dbGenerateInitialPlayerCreature,
     dbNewCreature,
     dbTurnCreature,
     dbStepCreature,
     dbGetCreatureScore,
     dbGetCreatureFaction)
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

dbGetCreatureScore :: (DBReadable db) => Score -> CreatureRef -> db Integer
dbGetCreatureScore score = liftM (creatureScore score) . dbGetCreature

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

