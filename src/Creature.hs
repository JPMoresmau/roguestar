{-# LANGUAGE PatternGuards #-}

module Creature 
    (generateInitialPlayerCreature,
     newCreature,
     Roll(..),
     RollComponents(..),
     rollCreatureAbilityScore,
     getCreatureFaction,
     injureCreature,
     healCreature,
     getCreatureHealth,
     getCreatureMaxHealth,
     getCreatureAbsoluteHealth,
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
import Facing
import Position
import Plane
import PlayerState

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

data RollComponents = RollComponents {
    component_base :: Integer,
    component_other_situation_bonus :: Integer,
    component_terrain_affinity_bonus :: Integer }

data Roll = Roll { 
    roll_ideal :: Integer,
    roll_actual :: Integer,
    roll_ideal_components :: RollComponents,
    roll_actual_components :: RollComponents,
    roll_log :: Integer }

rollCreatureAbilityScore :: (DBReadable db) => CreatureAbility -> Integer -> CreatureRef -> db Roll
rollCreatureAbilityScore score other_ideal creature_ref =
    do raw_ideal <- liftM (creatureAbilityScore score) $ dbGetCreature creature_ref
       terrain_ideal <- getTerrainAffinity creature_ref
       let ideal = raw_ideal + other_ideal + terrain_ideal
       actual <- linearRoll ideal
       [raw_actual, other_actual, terrain_actual] <- fixedSumLinearRoll [raw_ideal, other_ideal, terrain_ideal] actual
       logarithmic <- logRoll ideal
       --trace (show $ (score,raw_ideal,other_ideal,terrain_ideal,raw_actual,other_actual,terrain_actual)) $ return ()
       return $ Roll ideal (if raw_actual == 0 then 0 else actual)
                (RollComponents raw_ideal other_ideal terrain_ideal)
                (RollComponents raw_actual other_actual terrain_actual) logarithmic

-- | Ability bonus based on being good at working on specific types of terrain.
getTerrainAffinity :: (DBReadable db) => CreatureRef -> db Integer
getTerrainAffinity creature_ref =
    do l <- liftM (fmap location) $ getPlanarPosition creature_ref
       terrain_affinity_points <- case l of
           Nothing -> return 0
           Just (plane_ref,pos) -> liftM sum $ forM [minBound..maxBound] $ \face ->
               do t <- terrainAt plane_ref $ offsetPosition (facingToRelative face) pos
                  liftM (creatureAbilityScore $ TerrainAffinity t) $ dbGetCreature creature_ref
       return $ terrain_affinity_points `div` 4

getCreatureFaction :: (DBReadable db) => CreatureRef -> db Faction
getCreatureFaction = liftM creature_faction . dbGetCreature

injureCreature :: Integer -> CreatureRef -> DB ()
injureCreature x = dbModCreature $ \c -> c { creature_damage = max 0 $ creature_damage c + x }

healCreature :: Integer -> CreatureRef -> DB ()
healCreature = injureCreature . negate

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

-- | Delete all dead creatures from the database.
sweepDead :: Reference a -> DB ()
sweepDead ref =
    do worst_to_best_critters <- sortByRO getCreatureHealth =<< getDead ref
       flip mapM_ worst_to_best_critters $ \creature_ref ->
           do dbPushSnapshot (KilledEvent creature_ref)
	      deleteCreature creature_ref
