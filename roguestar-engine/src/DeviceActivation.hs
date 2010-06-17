module DeviceActivation
    (DeviceActivationOutcomeType(..),
     DeviceActivationOutcome(..),
     resolveDeviceActivation)
    where

import DB
import Creature
import CreatureData
import ToolData
import Data.Ratio

data DeviceActivationOutcomeType = 
    DeviceActivated
  | DeviceFailed
  | DeviceCriticalFailed

data DeviceActivationOutcome = DeviceActivationOutcome {
    dao_outcome_type :: DeviceActivationOutcomeType,
    dao_skill_roll :: Integer,
    dao_energy :: Integer,
    dao_activation_time :: Rational }

-- | Given a device, and a primary and secondary roll, determine the outcome of activating the device.
-- The better the primary roll, the less likely that the device will fail, while the better the secondary 
-- roll, the more energy the device will output.
resolveDeviceActivation :: (DBReadable db,DeviceType d) => CreatureAbility -> CreatureAbility -> CreatureAbility -> d -> CreatureRef -> db DeviceActivationOutcome
resolveDeviceActivation primary_ability secondary_ability timing_ability device creature_ref =
    do primary_roll <- rollCreatureAbilityScore primary_ability (deviceAccuracy device) creature_ref
       secondary_roll <- rollCreatureAbilityScore secondary_ability (deviceOutput device) creature_ref
       timing_roll <- rollCreatureAbilityScore timing_ability (deviceSpeed device) creature_ref
       let timing = roll_ideal secondary_roll % (roll_ideal timing_roll + roll_ideal secondary_roll)
           daoF = case () of
                      () | roll_actual primary_roll == 0 -> DeviceActivationOutcome DeviceCriticalFailed 0 (deviceOutput device * deviceSize device)
                      () | roll_actual primary_roll <= deviceSize device -> DeviceActivationOutcome DeviceFailed (roll_actual primary_roll) (deviceOutput device)
                      () | otherwise -> DeviceActivationOutcome DeviceActivated (roll_actual primary_roll * deviceSize device) (roll_actual secondary_roll)
       return $ daoF timing
