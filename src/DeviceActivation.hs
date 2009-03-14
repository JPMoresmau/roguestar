module DeviceActivation
    (DeviceActivationOutcomeType(..),
     DeviceActivationOutcome(..),
     resolveDeviceActivation)
    where

import DB
import Creature
import CreatureData
import ToolData

data DeviceActivationOutcomeType = 
    DeviceActivated
  | DeviceFailed
  | DeviceCriticalFailed

data DeviceActivationOutcome = DeviceActivationOutcome {
    dao_outcome_type :: DeviceActivationOutcomeType,
    dao_skill_roll :: Integer,
    dao_energy :: Integer,
    dao_primary_roll :: Roll,
    dao_secondary_roll :: Roll }

-- | Given a device, and a primary and secondary roll, determine the outcome of activating the device.
-- The better the primary roll, the less likely that the device will fail, while the better the secondary 
-- roll, the more energy the device will output.
resolveDeviceActivation :: (DBReadable db,DeviceType d) => CreatureAbility -> CreatureAbility -> d -> CreatureRef -> db DeviceActivationOutcome
resolveDeviceActivation primary secondary device creature_ref =
    do primary_roll <- rollCreatureAbilityScore primary 0 creature_ref
       secondary_roll <- rollCreatureAbilityScore secondary (deviceEnergyOutput device) creature_ref
       physical_energy <- linearRoll $ min (roll_actual secondary_roll) (deviceEnergyOutput device)
       energy_throughput <- linearRoll $ deviceThroughput device
       let physical_energy_delivered = min physical_energy energy_throughput
       let overheat_energy = physical_energy - physical_energy_delivered
       let daoF = case () of
                      () | roll_actual primary_roll < deviceSize device -> DeviceActivationOutcome DeviceCriticalFailed 0 physical_energy
                      () | roll_actual primary_roll < overheat_energy -> DeviceActivationOutcome DeviceFailed 0 overheat_energy
                      () | otherwise -> DeviceActivationOutcome DeviceActivated (roll_actual primary_roll * deviceSize device) (roll_actual secondary_roll)
       return $ daoF primary_roll secondary_roll

