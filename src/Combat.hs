{-# LANGUAGE PatternGuards, FlexibleContexts #-}

module Combat
    (AttackModel,
     meleeAttackModel,
     rangedAttackModel,
     resolveAttack,
     executeAttack)
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
import Data.List
import Data.Ord
import DeviceActivation
import Contact
import Plane

data AttackModel =
    RangedAttackModel CreatureRef ToolRef Device
  | MeleeAttackModel CreatureRef ToolRef Device
  | UnarmedAttackModel CreatureRef

attacker :: AttackModel -> CreatureRef
attacker (RangedAttackModel attacker_ref _ _) = attacker_ref
attacker (MeleeAttackModel attacker_ref _ _) = attacker_ref
attacker (UnarmedAttackModel attacker_ref) = attacker_ref

weapon :: AttackModel -> Maybe ToolRef
weapon (RangedAttackModel _ weapon_ref _) = Just weapon_ref
weapon (MeleeAttackModel _ weapon_ref _) = Just weapon_ref
weapon (UnarmedAttackModel {}) = Nothing

instance DeviceType AttackModel where
    toPseudoDevice (RangedAttackModel _ _ d) = toPseudoDevice d
    toPseudoDevice (MeleeAttackModel _ _ d) = toPseudoDevice d
    toPseudoDevice (UnarmedAttackModel {}) = PseudoDevice 0 0 0 1

interactionMode :: AttackModel -> CreatureInteractionMode
interactionMode (RangedAttackModel {}) = Ranged
interactionMode (MeleeAttackModel {}) = Melee
interactionMode (UnarmedAttackModel {}) = Unarmed

-- | Get the attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a weapon.
attackModel :: (DBReadable db) => CreatureRef -> db AttackModel
attackModel attacker_ref =
    do m_tool_ref <- dbGetWielded attacker_ref
       case m_tool_ref of
           Nothing -> return $ UnarmedAttackModel attacker_ref
           Just tool_ref ->
               do tool <- dbGetTool tool_ref
                  case tool of
                      DeviceTool Gun device -> return $ RangedAttackModel attacker_ref tool_ref device
                      DeviceTool Sword device -> return $ MeleeAttackModel attacker_ref tool_ref device
                      _ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"

-- | Get an appropriate melee attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a suitable melee weapon.
meleeAttackModel :: (DBReadable db) => CreatureRef -> db AttackModel
meleeAttackModel attacker_ref =
    do attack_model <- attackModel attacker_ref
       case interactionMode attack_model `elem` [Melee,Unarmed] of
           True -> return attack_model
           _ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"

-- | Get an appropriate ranged attack model for a creature, based on whatever tool the creature is holding.
-- This will fail if the creature is holding anything other than a suitable ranged or splash weapon.
rangedAttackModel :: (DBReadable db) => CreatureRef -> db AttackModel
rangedAttackModel attacker_ref =
    do attack_model <- attackModel attacker_ref
       case interactionMode attack_model `elem` [Ranged,Splash] of
           True -> return attack_model
           _ -> throwError $ DBErrorFlag "innapropriate-tool-wielded"

data AttackOutcome =
    AttackMiss CreatureRef (Maybe ToolRef)
  | AttackMalfunction CreatureRef ToolRef Integer
  | AttackExplodes CreatureRef ToolRef Integer
  | AttackHit CreatureRef (Maybe ToolRef) CreatureRef Integer
  | AttackDisarm CreatureRef CreatureRef ToolRef
  | AttackSunder CreatureRef ToolRef CreatureRef ToolRef

resolveAttack :: (DBReadable db) => AttackModel -> Facing -> db AttackOutcome
resolveAttack attack_model face =
    do device_activation <- resolveDeviceActivation (AttackSkill $ interactionMode attack_model) 
                                                    (DamageSkill $ interactionMode attack_model)
                                                    (ReloadSkill $ interactionMode attack_model)
                                                    (toPseudoDevice attack_model)
                                                    (attacker attack_model)
       m_defender_ref <- liftM listToMaybe $ findContacts (contactMode $ interactionMode attack_model) (attacker attack_model) face
       case (dao_outcome_type device_activation,m_defender_ref) of
           (DeviceFailed, _) | Just tool_ref <- weapon attack_model -> 
               return $ AttackMalfunction (attacker attack_model) tool_ref (dao_energy device_activation)
           (DeviceCriticalFailed, _) | Just tool_ref <- weapon attack_model ->
               return $ AttackExplodes (attacker attack_model) tool_ref (dao_energy device_activation)
           (DeviceActivated, Just defender_ref) ->
               do defense_outcome <- resolveDefense (interactionMode attack_model) defender_ref
                  distance_squared <- liftM (fromMaybe 0) $ dbDistanceBetweenSquared (attacker attack_model) defender_ref
                  let isDisarmingBlow = dao_skill_roll device_activation > do_skill_roll defense_outcome + distance_squared &&
                                        dao_energy device_activation > do_damage_reduction defense_outcome + do_disarm_bonus defense_outcome
                  case () of
                      () | dao_skill_roll device_activation <= do_skill_roll defense_outcome + distance_squared ->
                           return $ AttackMiss (attacker attack_model) (weapon attack_model)
                      () | isDisarmingBlow && interactionMode attack_model == Unarmed,
                           Just defender_wield_ref <- do_defender_wield defense_outcome ->
                               return $ AttackDisarm (attacker attack_model) defender_ref defender_wield_ref
                      () | isDisarmingBlow && interactionMode attack_model == Melee,
                           Just weapon_ref <- weapon attack_model,
                           Just defender_wield_ref <- do_defender_wield defense_outcome ->
                               return $ AttackSunder (attacker attack_model) weapon_ref defender_ref defender_wield_ref
                      () -> return $ AttackHit (attacker attack_model) (weapon attack_model) defender_ref (max 0 $ dao_energy device_activation - do_damage_reduction defense_outcome)
           _ -> return $ AttackMiss (attacker attack_model) (weapon attack_model)

data DefenseOutcome = DefenseOutcome {
    do_defender_wield :: Maybe ToolRef,
    do_skill_roll :: Integer,
    do_damage_reduction :: Integer,
    do_disarm_bonus :: Integer }

resolveDefense :: (DBReadable db) => CreatureInteractionMode -> CreatureRef -> db DefenseOutcome                                                                                             
resolveDefense interaction_mode defender_ref =
    do m_tool_ref <- dbGetWielded defender_ref
       m_tool <- maybe (return Nothing) (liftM Just . dbGetTool) m_tool_ref
       disarm_bonus <- maybe (return 0) toolDurability m_tool_ref
       let pdevice = case m_tool of
               Just (DeviceTool Sword d) | interaction_mode `elem` [Melee,Unarmed] -> toPseudoDevice d
               _ -> PseudoDevice 0 0 0 1
       device_activation <- resolveDeviceActivation (DefenseSkill interaction_mode)
                                                    (DamageReductionTrait interaction_mode)
                                                    InventorySkill
                                                    pdevice
                                                    defender_ref
       return $ case dao_outcome_type device_activation of
          DeviceActivated -> DefenseOutcome m_tool_ref (dao_skill_roll device_activation) (dao_energy device_activation) disarm_bonus
          DeviceFailed -> DefenseOutcome m_tool_ref 0 0 disarm_bonus
          DeviceCriticalFailed -> DefenseOutcome m_tool_ref 0 0 0

executeAttack :: AttackOutcome -> DB ()
executeAttack (AttackMiss attacker_ref m_tool_ref) =
    do dbPushSnapshot $ MissEvent attacker_ref m_tool_ref
executeAttack (AttackHit attacker_ref m_tool_ref defender_ref damage) =
    do dbPushSnapshot $ AttackEvent attacker_ref m_tool_ref defender_ref
       injureCreature damage defender_ref
executeAttack (AttackMalfunction attacker_ref tool_ref damage) =
    do dbPushSnapshot $ WeaponOverheatsEvent attacker_ref tool_ref
       injureCreature damage attacker_ref
       dbMove dbDropTool tool_ref
       return ()
executeAttack (AttackExplodes attacker_ref tool_ref damage) =
    do dbPushSnapshot $ WeaponExplodesEvent attacker_ref tool_ref
       injureCreature damage attacker_ref
       deleteTool tool_ref
executeAttack (AttackDisarm attacker_ref defender_ref dropped_tool) =
    do dbPushSnapshot $ DisarmEvent attacker_ref defender_ref dropped_tool
       dbMove dbDropTool dropped_tool
       return ()
executeAttack (AttackSunder attacker_ref weapon_ref defender_ref sundered_tool) =
    do dbPushSnapshot $ SunderEvent attacker_ref weapon_ref defender_ref sundered_tool
       deleteTool sundered_tool

