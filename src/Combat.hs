{-# LANGUAGE PatternGuards, FlexibleContexts #-}

module Combat
    (dbResolveRangedAttack,
     dbResolveMeleeAttack,
     dbExecuteRangedAttack,
     dbExecuteMeleeAttack)
    where

import DB
import DBData
import Creature
import CreatureData
import Tool
import ToolData
import Control.Monad.Error
import Facing
import Data.Maybe
import Plane
import Dice
import Data.List
import Data.Ord
import Position

data RangedAttackOutcome =
    RangedAttackMiss CreatureRef ToolRef
  | RangedAttackHitCreature CreatureRef ToolRef CreatureRef Integer

dbResolveRangedAttack :: (DBReadable db) => CreatureRef -> Facing -> db RangedAttackOutcome
dbResolveRangedAttack attacker_ref face =
    do m_defender_ref <- liftM listToMaybe $ dbFindRangedTargets attacker_ref face
       tool_ref <- maybe (throwError $ DBErrorFlag "no-weapon-wielded") return =<< dbGetWielded attacker_ref
       attack_roll <- dbRollRangedAttack attacker_ref
       damage_roll <- dbRollRangedDamage attacker_ref tool_ref
       case m_defender_ref of
           Nothing -> return $ RangedAttackMiss attacker_ref tool_ref
           Just defender_ref ->
	       do defense_roll <- dbRollRangedDefense attacker_ref defender_ref
                  injury_roll <- rollInjury Ranged defender_ref damage_roll
		  return $ case () of
		      () | attack_roll > defense_roll -> RangedAttackHitCreature attacker_ref tool_ref defender_ref injury_roll
		      () | otherwise -> RangedAttackMiss attacker_ref tool_ref

data MeleeAttackOutcome =
    UnarmedAttackHitCreature CreatureRef CreatureRef Integer
  | UnarmedAttackMiss CreatureRef

dbResolveMeleeAttack :: (DBReadable db) => CreatureRef -> Facing -> db MeleeAttackOutcome
dbResolveMeleeAttack attacker_ref face =
    do m_defender_ref <- liftM listToMaybe $ dbFindMeleeTargets attacker_ref face
       attack_roll <- dbRollMeleeAttack attacker_ref
       damage_roll <- dbRollMeleeDamage attacker_ref
       case m_defender_ref of
           Nothing -> return $ UnarmedAttackMiss attacker_ref
	   Just defender_ref ->
	       do defense_roll <- dbRollMeleeDefense attacker_ref defender_ref
	          injury_roll <- rollInjury Melee defender_ref damage_roll
		  return $ case () of
		      () | attack_roll > defense_roll -> UnarmedAttackHitCreature attacker_ref defender_ref injury_roll
		      () | otherwise -> UnarmedAttackMiss attacker_ref 

dbExecuteRangedAttack :: RangedAttackOutcome -> DB ()
dbExecuteRangedAttack (RangedAttackMiss attacker_ref tool_ref) = 
    do dbPushSnapshot (MissEvent attacker_ref (Just tool_ref))
dbExecuteRangedAttack (RangedAttackHitCreature attacker_ref tool_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref (Just tool_ref) defender_ref)
       injureCreature damage defender_ref
       sweepDead =<< liftM getLocation (dbWhere attacker_ref)

dbExecuteMeleeAttack :: MeleeAttackOutcome -> DB ()
dbExecuteMeleeAttack (UnarmedAttackMiss attacker_ref) =
    do dbPushSnapshot (MissEvent attacker_ref Nothing)
dbExecuteMeleeAttack (UnarmedAttackHitCreature attacker_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref Nothing defender_ref)
       injureCreature damage defender_ref
       sweepDead =<< liftM getLocation (dbWhere attacker_ref)

dbRollRangedDamage :: (DBReadable db) => CreatureRef -> ToolRef -> db Integer
dbRollRangedDamage _ weapon_ref =
    do tool <- dbGetTool weapon_ref
       case tool of
           GunTool g ->
	       do energy_released <- roll [0..gunEnergyOutput g]
	          energy_throughput <- roll [0..gunThroughput g] -- todo: overheats if energy_released > energy_throughput
		  return $ min energy_released energy_throughput

dbRollMeleeDamage :: (DBReadable db) => CreatureRef -> db Integer
dbRollMeleeDamage attacker_ref = liftM roll_actual $ rollCreatureAbilityScore (DamageSkill Melee) 0 attacker_ref

dbRollRangedAttack :: (DBReadable db) => CreatureRef -> db Integer
dbRollRangedAttack attacker_ref = liftM roll_actual $ rollCreatureAbilityScore (AttackSkill Melee) 0 attacker_ref

dbRollMeleeAttack :: (DBReadable db) => CreatureRef -> db Integer
dbRollMeleeAttack attacker_ref = liftM roll_actual $ rollCreatureAbilityScore (AttackSkill Melee) 0 attacker_ref

dbRollRangedDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
dbRollRangedDefense attacker_ref x_defender_ref =
    do distance <- liftM (fromMaybe (error "dbGetOpposedAttackRoll: defender and attacker are on different planes")) $ dbDistanceBetweenSquared attacker_ref x_defender_ref 
       case () of
           () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Ranged) distance defender_ref
	   () | otherwise -> return distance
       
dbRollMeleeDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
dbRollMeleeDefense _ x_defender_ref = 
    case () of
        () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Melee) 0 defender_ref
        () | otherwise -> return 1

dbFindRangedTargets :: (DBReadable db,ReferenceType x,GenericReference a S) => Reference x -> Facing -> db [a]
dbFindRangedTargets attacker_ref face =
    do m_l <- liftM (fmap location) $ getPlanarLocation attacker_ref
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (mapMaybe fromLocation .
	          sortBy (comparing (distanceBetweenSquared pos . location)) .
	          filter ((/= generalizeReference attacker_ref) . entity) . 
	          filter (isFacing (pos,face) . location)) $ 
		      dbGetContents plane_ref

dbFindMeleeTargets :: (DBReadable db,ReferenceType x,GenericReference a S) => Reference x -> Facing -> db [a]
dbFindMeleeTargets attacker_ref face =
    do m_l <- liftM (fmap location) $ getPlanarLocation attacker_ref
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (mapMaybe fromLocation .
	          filter (\x -> (location x == (offsetPosition (facingToRelative face) pos) || location x == pos) &&
	                        generalizeReference attacker_ref /= entity x)) $
	       dbGetContents plane_ref
