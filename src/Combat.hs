{-# LANGUAGE PatternGuards, FlexibleContexts #-}

module Combat
    (RangedAttackModel,
     rangedAttackWithWeapon,
     resolveRangedAttack,
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
import Control.Monad.Maybe

data RangedAttackOutcome =
    RangedAttackMiss CreatureRef ToolRef
  | RangedAttackHitCreature CreatureRef ToolRef CreatureRef Integer
  | RangedAttackOverheats CreatureRef ToolRef Integer
  | RangedAttackCriticalFail CreatureRef ToolRef Integer

data RangedAttackModel = WithWeapon CreatureRef ToolRef Device deriving (Read,Show)

rangedAttackWithWeapon :: (DBReadable db) => CreatureRef -> db RangedAttackModel
rangedAttackWithWeapon attacker_ref =
    do tool_ref <- maybe (throwError $ DBErrorFlag "no-weapon-wielded") return =<< dbGetWielded attacker_ref
       tool <- dbGetTool tool_ref
       case tool of
           DeviceTool Gun device -> return $ WithWeapon attacker_ref tool_ref device
           _ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"

resolveRangedAttack :: (DBReadable db) => RangedAttackModel -> Facing -> db RangedAttackOutcome
resolveRangedAttack (WithWeapon attacker_ref tool_ref device) face =
    do device_activation_outcome <- resolveDeviceActivation (AttackSkill Ranged) (DamageSkill Ranged) (ReloadSkill Ranged) device attacker_ref
       m_defender_ref <- liftM listToMaybe $ findRangedTargets attacker_ref face
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
  | UnarmedAttackDisarm CreatureRef CreatureRef ToolRef
  | ArmedAttackHitCreature CreatureRef ToolRef CreatureRef Integer
  | ArmedAttackSunder CreatureRef ToolRef CreatureRef ToolRef
  | ArmedAttackMiss CreatureRef ToolRef
  | ArmedAttackOverheats CreatureRef ToolRef Integer
  | ArmedAttackCriticalFail CreatureRef ToolRef Integer

resolveMeleeAttack :: (DBReadable db) => CreatureRef -> Facing -> db MeleeAttackOutcome
resolveMeleeAttack attacker_ref face =
    do m_defender_ref <- liftM listToMaybe $ findMeleeTargets attacker_ref face
       m_tool_ref <- dbGetWielded attacker_ref
       case (m_defender_ref,m_tool_ref) of
           -- unarmed attack against thin air
           (Nothing,Nothing) -> return $ UnarmedAttackMiss attacker_ref
           -- unarmed attack against a creature
           (Just defender_ref,Nothing) ->
               do attack_roll <- liftM roll_actual $ rollCreatureAbilityScore (AttackSkill Unarmed) 0 attacker_ref
                  damage_roll <- liftM roll_actual $ rollCreatureAbilityScore (DamageSkill Unarmed) 0 attacker_ref
                  defense_roll <- rollMeleeDefense attacker_ref defender_ref
                  injury_roll <- rollInjury Unarmed defender_ref damage_roll
                  m_defender_wield <- dbGetWielded defender_ref
                  case m_defender_wield of
                      (Just defender_wield_ref) | attack_roll > defense_roll && injury_roll > 0 -> return $ UnarmedAttackDisarm attacker_ref defender_ref defender_wield_ref
                      _ | attack_roll > defense_roll -> return $ UnarmedAttackHitCreature attacker_ref defender_ref injury_roll
                      _ | otherwise -> return $ UnarmedAttackMiss attacker_ref
           -- armed attack
           (_,Just tool_ref) ->
               do tool <- dbGetTool tool_ref
                  device_activation_outcome <- case tool of
                      DeviceTool Sword device -> resolveDeviceActivation (AttackSkill Melee) (DamageSkill Melee) (ReloadSkill Melee) device attacker_ref
                      _ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"
                  case (m_defender_ref, dao_outcome_type device_activation_outcome) of
                      (_,DeviceCriticalFailed) -> return $ ArmedAttackCriticalFail attacker_ref tool_ref (dao_energy device_activation_outcome)
                      (_,DeviceFailed) -> return $ ArmedAttackOverheats attacker_ref tool_ref (dao_energy device_activation_outcome)
                      (Nothing,_) -> return $ ArmedAttackMiss attacker_ref tool_ref
                      (Just defender_ref,DeviceActivated) ->
                          do defense_roll <- rollMeleeDefense attacker_ref defender_ref
                             injury_roll <- rollInjury Ranged defender_ref (dao_energy device_activation_outcome)
                             m_defender_wield_info <- runMaybeT $
                                 do defender_wield_ref <- MaybeT $ dbGetWielded defender_ref
                                    defender_wield_durability <- lift $ toolDurability defender_wield_ref
                                    return (defender_wield_ref,defender_wield_durability)
                             case m_defender_wield_info of
                                 Just (defender_wield_ref,durability) | roll_actual (dao_primary_roll device_activation_outcome) > defense_roll && injury_roll > durability -> 
                                     return $ ArmedAttackSunder attacker_ref tool_ref defender_ref defender_wield_ref
                                 _ | dao_skill_roll device_activation_outcome > defense_roll -> return $ ArmedAttackHitCreature attacker_ref tool_ref defender_ref injury_roll
                                 _ | otherwise -> return $ ArmedAttackMiss attacker_ref tool_ref 

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
executeMeleeAttack (UnarmedAttackHitCreature attacker_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref Nothing defender_ref)
       injureCreature damage defender_ref
executeMeleeAttack (UnarmedAttackMiss attacker_ref) =
    do dbPushSnapshot (MissEvent attacker_ref Nothing)
executeMeleeAttack (UnarmedAttackDisarm attacker_ref defender_ref dropped_tool) =
    do dbPushSnapshot (DisarmEvent attacker_ref defender_ref dropped_tool)
       dbMove dbDropTool dropped_tool
       return ()
executeMeleeAttack (ArmedAttackHitCreature attacker_ref weapon_ref defender_ref damage) =
    do dbPushSnapshot (AttackEvent attacker_ref (Just weapon_ref) defender_ref)
       injureCreature damage defender_ref
executeMeleeAttack (ArmedAttackSunder attacker_ref weapon_ref defender_ref sundered_tool) =
    do dbPushSnapshot (SunderEvent attacker_ref weapon_ref defender_ref sundered_tool)
       deleteTool sundered_tool
executeMeleeAttack (ArmedAttackMiss attacker_ref weapon_ref) =
    do dbPushSnapshot (MissEvent attacker_ref $ Just weapon_ref)
executeMeleeAttack (ArmedAttackOverheats attacker_ref weapon_ref damage) =
    do dbPushSnapshot (WeaponOverheatsEvent attacker_ref weapon_ref)
       injureCreature damage attacker_ref
executeMeleeAttack (ArmedAttackCriticalFail attacker_ref weapon_ref damage) =
    do dbPushSnapshot (WeaponExplodesEvent attacker_ref weapon_ref)
       injureCreature damage attacker_ref
       deleteTool weapon_ref

rollRangedDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
rollRangedDefense attacker_ref x_defender_ref =
    do distance <- liftM (fromMaybe (error "dbGetOpposedAttackRoll: defender and attacker are on different planes")) $ dbDistanceBetweenSquared attacker_ref x_defender_ref 
       case () of
           () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Ranged) distance defender_ref
	   () | otherwise -> return distance
       
rollMeleeDefense :: (DBReadable db,ReferenceType a) => CreatureRef -> Reference a -> db Integer
rollMeleeDefense _ x_defender_ref = 
    case () of
        () | Just defender_ref <- coerceReferenceTyped _creature x_defender_ref -> 
            do m_defender_wield <- dbGetWielded defender_ref
               case m_defender_wield of
                   Nothing -> liftM roll_actual $ rollCreatureAbilityScore (DefenseSkill Unarmed) 0 defender_ref
                   Just defender_wield -> 
                       do t <- dbGetTool defender_wield
                          let defense_multiplier = case t of
                                                       DeviceTool Sword d -> deviceSize d
                                                       _ -> 1
                          liftM ((* defense_multiplier) . roll_actual) $ rollCreatureAbilityScore (DefenseSkill Melee) 0 defender_ref
                              
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
