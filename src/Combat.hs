{-# LANGUAGE PatternGuards, FlexibleContexts #-}

module Combat
    (resolveRangedAttack,
     resolveMeleeAttack,
     executeRangedAttack,
     executeMeleeAttack,
     RangedAttackOutcome,
     MeleeAttackOutcome)
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
import Data.List
import Data.Ord
import Position
import DeviceActivation

data RangedAttackOutcome =
    RangedAttackMiss CreatureRef ToolRef
  | RangedAttackHitCreature CreatureRef ToolRef CreatureRef Integer
  | RangedAttackOverheats CreatureRef ToolRef Integer
  | RangedAttackCriticalFail CreatureRef ToolRef Integer

resolveRangedAttack :: (DBReadable db) => CreatureRef -> Facing -> db RangedAttackOutcome
resolveRangedAttack attacker_ref face =
    do m_defender_ref <- liftM listToMaybe $ findRangedTargets attacker_ref face
       tool_ref <- maybe (throwError $ DBErrorFlag "no-weapon-wielded") return =<< dbGetWielded attacker_ref
       tool <- dbGetTool tool_ref
       device_activation_outcome <- case tool of
           DeviceTool Gun device -> resolveDeviceActivation (AttackSkill Ranged) (DamageSkill Ranged) device attacker_ref
           --_ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"
       case (m_defender_ref, dao_outcome_type device_activation_outcome) of
           (_,DeviceCriticalFailed) -> return $ RangedAttackCriticalFail attacker_ref tool_ref (dao_energy device_activation_outcome)
           (_,DeviceFailed) -> return $ RangedAttackOverheats attacker_ref tool_ref (dao_energy device_activation_outcome)
           (Nothing,_) -> return $ RangedAttackMiss attacker_ref tool_ref
           (Just defender_ref,DeviceActivated) ->
               do defense_roll <- rollRangedDefense attacker_ref defender_ref
                  injury_roll <- rollInjury Ranged defender_ref (dao_energy device_activation_outcome)
                  case () of
                      () | dao_skill_roll device_activation_outcome > defense_roll -> return $ RangedAttackHitCreature attacker_ref tool_ref defender_ref injury_roll
                      () | otherwise -> return $ RangedAttackMiss attacker_ref tool_ref

data MeleeAttackOutcome =
    UnarmedAttackHitCreature CreatureRef CreatureRef Integer
  | UnarmedAttackMiss CreatureRef

resolveMeleeAttack :: (DBReadable db) => CreatureRef -> Facing -> db MeleeAttackOutcome
resolveMeleeAttack attacker_ref face =
    do m_defender_ref <- liftM listToMaybe $ findMeleeTargets attacker_ref face
       attack_roll <- rollMeleeAttack attacker_ref
       damage_roll <- rollMeleeDamage attacker_ref
       case m_defender_ref of
           Nothing -> return $ UnarmedAttackMiss attacker_ref     
	   Just defender_ref ->
	       do defense_roll <- rollMeleeDefense attacker_ref defender_ref
	          injury_roll <- rollInjury Melee defender_ref damage_roll
		  return $ case () of
		      () | attack_roll > defense_roll -> UnarmedAttackHitCreature attacker_ref defender_ref injury_roll
		      () | otherwise -> UnarmedAttackMiss attacker_ref 

executeRangedAttack :: RangedAttackOutcome -> DB ()
executeRangedAttack (RangedAttackMiss attacker_ref tool_ref) = 
    do dbPushSnapshot (MissEvent attacker_ref (Just tool_ref))
executeRangedAttack (RangedAttackHitCreature attacker_ref tool_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref (Just tool_ref) defender_ref)
       injureCreature damage defender_ref
executeRangedAttack (RangedAttackOverheats attacker_ref tool_ref damage) =
    do dbPushSnapshot (WeaponOverheatsEvent attacker_ref tool_ref)
       injureCreature damage attacker_ref
executeRangedAttack (RangedAttackCriticalFail attacker_ref tool_ref damage) =
    do dbPushSnapshot (WeaponExplodesEvent attacker_ref tool_ref)
       injureCreature damage attacker_ref
       deleteTool tool_ref

executeMeleeAttack :: MeleeAttackOutcome -> DB ()
executeMeleeAttack (UnarmedAttackMiss attacker_ref) =
    do dbPushSnapshot (MissEvent attacker_ref Nothing)
executeMeleeAttack (UnarmedAttackHitCreature attacker_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref Nothing defender_ref)
       injureCreature damage defender_ref

rollMeleeDamage :: (DBReadable db) => CreatureRef -> db Integer
rollMeleeDamage attacker_ref = liftM roll_actual $ rollCreatureAbilityScore (DamageSkill Melee) 0 attacker_ref

rollMeleeAttack :: (DBReadable db) => CreatureRef -> db Integer
rollMeleeAttack attacker_ref = liftM roll_actual $ rollCreatureAbilityScore (AttackSkill Melee) 0 attacker_ref

rollRangedDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
rollRangedDefense attacker_ref x_defender_ref =
    do distance <- liftM (fromMaybe (error "dbGetOpposedAttackRoll: defender and attacker are on different planes")) $ dbDistanceBetweenSquared attacker_ref x_defender_ref 
       case () of
           () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Ranged) distance defender_ref
	   () | otherwise -> return distance
       
rollMeleeDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
rollMeleeDefense _ x_defender_ref = 
    case () of
        () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Melee) 0 defender_ref
        () | otherwise -> return 1

findRangedTargets :: (DBReadable db,ReferenceType x,GenericReference a S) => Reference x -> Facing -> db [a]
findRangedTargets attacker_ref face =
    do m_l <- liftM (fmap location) $ getPlanarLocation attacker_ref
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (mapMaybe fromLocation .
	          sortBy (comparing (distanceBetweenSquared pos . location)) .
	          filter ((/= generalizeReference attacker_ref) . entity) . 
	          filter (isFacing (pos,face) . location)) $ 
		      dbGetContents plane_ref

findMeleeTargets :: (DBReadable db,ReferenceType x,GenericReference a S) => Reference x -> Facing -> db [a]
findMeleeTargets attacker_ref face =
    do m_l <- liftM (fmap location) $ getPlanarLocation attacker_ref
       flip (maybe $ return []) m_l $ \(plane_ref,pos) ->
           liftM (mapMaybe fromLocation .
	          filter (\x -> (location x == (offsetPosition (facingToRelative face) pos) || location x == pos) &&
	                        generalizeReference attacker_ref /= entity x)) $
	       dbGetContents plane_ref
