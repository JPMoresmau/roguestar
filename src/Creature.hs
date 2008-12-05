{-# LANGUAGE PatternGuards #-}

module Creature 
    (generateInitialPlayerCreature,
     newCreature,
     Roll(..),
     rollCreatureAbilityScore,
     getCreatureFaction,
     rollInjury,
     injureCreature,
     getCreatureHealth,
     getDead,
     deleteCreature,
     sweepDead)
    where

import Data.Maybe
import CreatureData
import DB
import SpeciesData
import Species
import DBData
import FactionData
import Control.Monad.Error
import Tool
import CreatureAttribute
import Data.Monoid
import Data.Ratio

-- |
-- Generates a new Creature from the specified species.
--
generateCreature :: Faction -> Species -> DB Creature
generateCreature faction species = generateAttributes faction species $ mconcat $ species_starting_attributes $ speciesInfo species

-- |
-- During DBRaceSelectionState, generates a new Creature for the player character and sets it into the 
-- database's DBClassSelectionState.
--
generateInitialPlayerCreature :: Species -> DB ()
generateInitialPlayerCreature species = 
    do newc <- generateCreature Player species
       dbSetStartingRace species
       setPlayerState (ClassSelectionState newc)

-- |
-- Generates a new Creature from the specified Species and adds it to the database.
--
newCreature :: (CreatureLocation l) => Faction -> Species -> l -> DB CreatureRef
newCreature faction species loc = 
    do creature <- generateCreature faction species
       dbAddCreature creature loc

data Roll = Roll { 
    roll_ideal :: Integer,
    roll_other_situation_bonus :: Integer,
    roll_actual :: Integer,
    roll_log :: Integer }

rollCreatureAbilityScore :: (DBReadable db) => CreatureAbility -> Integer -> CreatureRef -> db Roll
rollCreatureAbilityScore score bonus creature_ref =
    do ideal <- liftM ((+ bonus) . creatureAbilityScore score) $ dbGetCreature creature_ref
       actual <- linearRoll ideal
       logarithmic <- logRoll ideal
       return $ Roll ideal bonus actual logarithmic

getCreatureFaction :: (DBReadable db) => CreatureRef -> db Faction
getCreatureFaction = liftM creature_faction . dbGetCreature

-- | Roll the amount of damage taken by a creature.  The input is the amount of damage recieved,
-- while the output is the amount of damage actually absorbed after damage reduction.
-- A critical failure results in double damage.
rollInjury :: (DBReadable db) => CreatureInteractionMode -> CreatureRef -> Integer -> db Integer
rollInjury interaction_mode creature_ref damage_roll = 
    do damage_reduction <- liftM roll_actual $ rollCreatureAbilityScore (DamageReductionTrait interaction_mode) 0 creature_ref
       return $ case damage_reduction of
           0 -> damage_roll * 2
           _ -> max 0 $ damage_roll - damage_reduction
       
injureCreature :: Integer -> CreatureRef -> DB ()
injureCreature x = dbModCreature $ \c -> c { creature_damage = creature_damage c + x }

getCreatureMaxHealth :: (DBReadable db) => CreatureRef -> db Integer
getCreatureMaxHealth = liftM (creatureAbilityScore ToughnessTrait) . dbGetCreature

-- | Injury difference from maximum health as an integer count of hit points.
getCreatureInjury :: (DBReadable db) => CreatureRef -> db Integer
getCreatureInjury = liftM creature_damage . dbGetCreature

-- | Health as an integer count of hit points.
getCreatureAbsoluteHealth :: (DBReadable db) => CreatureRef -> db Integer
getCreatureAbsoluteHealth creature_ref = liftM2 (-) (getCreatureMaxHealth creature_ref) (getCreatureInjury creature_ref)

-- | Health as a fraction of 1.
getCreatureHealth :: (DBReadable db) => CreatureRef -> db Rational
getCreatureHealth creature_ref = liftM2 (%) (getCreatureAbsoluteHealth creature_ref) (getCreatureMaxHealth creature_ref)

getDead :: (DBReadable db) => Reference a -> db [CreatureRef]
getDead parent_ref = filterRO (liftM (<= 0) . getCreatureHealth) =<< dbGetContents parent_ref

deleteCreature :: CreatureRef -> DB ()
deleteCreature = dbUnsafeDeleteObject $ \l ->
    do m_dropped_loc <- maybe (return Nothing) (liftM Just . dbDropTool) $ coerceEntityTyped _tool l
       return $ case m_dropped_loc of
           Just dropped_loc -> generalizeLocationRecord dropped_loc
	   Nothing -> error "dbDeleteCreature: no case for this type of entity"

-- | Delete all dead creature from the database.
sweepDead :: Reference a -> DB ()
sweepDead ref =
    do worst_to_best_critters <- sortByRO getCreatureHealth =<< getDead ref
       flip mapM_ worst_to_best_critters $ \creature_ref ->
           do dbPushSnapshot (KilledEvent creature_ref)
	      deleteCreature creature_ref
