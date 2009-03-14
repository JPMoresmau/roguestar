module ToolData 
    (Tool(..),
     Device,
     DeviceKind(..),
     DeviceType(..),
     deviceEnergyOutput,
     deviceThroughput,
     deviceDurability,
     deviceSize,
     toDevice,
     toolName,
     phase_pistol,
     phaser,
     phase_rifle,
     kinetic_fleuret,
     kinetic_sabre)
    where

import Substances

data Tool = DeviceTool DeviceKind Device
    deriving (Read,Show,Eq)

data DeviceKind = Gun | Sword
            deriving (Read,Show,Eq)

-- | Any kind of device that is constructed from a power cell, materal, and gas medium,
-- using the various device rules to determine it's power.
data Device = Device {
   device_name :: String,
   device_power_cell :: Chromalite,
   device_material :: Material,
   device_medium :: Gas,
   device_size :: Integer }
     deriving (Eq,Read,Show)

-- | Anything that operates like a device, but isn't.  For example, an unarmed attack.
data PseudoDevice = PseudoDevice {
    pdevice_energy_output :: Integer,
    pdevice_throughput :: Integer,
    pdevice_size :: Integer }

class DeviceType d where
    toPseudoDevice :: d -> PseudoDevice

instance DeviceType Device where
    toPseudoDevice d = PseudoDevice {
        pdevice_energy_output = device_size d * (chromalitePotency $ device_power_cell d),
        pdevice_throughput = ((material_critical_value $ materialValue $ device_material d) + 1) * (gasWeight $ device_medium d),
        pdevice_size = device_size d }

instance DeviceType PseudoDevice where
    toPseudoDevice = id

gun :: Device -> Tool
gun = DeviceTool Gun

sword :: Device -> Tool
sword = DeviceTool Sword

phase_pistol :: Tool
phase_pistol = gun $ Device "phase_pistol" Pteulanium Zinc Argon 1

phaser :: Tool
phaser = gun $ Device "phaser" Pteulanium Zinc Argon 3

phase_rifle :: Tool
phase_rifle = gun $ Device "phase_rifle" Pteulanium Zinc Argon 5

kinetic_fleuret :: Tool
kinetic_fleuret = sword $ Device "kinetic_fleuret" Ionidium Aluminum Nitrogen 2

kinetic_sabre :: Tool
kinetic_sabre = sword $ Device "kinetic_sabre" Ionidium Aluminum Nitrogen 4

deviceEnergyOutput :: (DeviceType d) => d -> Integer
deviceEnergyOutput = pdevice_energy_output . toPseudoDevice

deviceThroughput :: (DeviceType d) => d -> Integer
deviceThroughput = pdevice_throughput . toPseudoDevice

deviceDurability :: Device -> Integer
deviceDurability d = device_size d * (material_construction_value $ materialValue $ device_material d)

deviceSize :: (DeviceType d) => d -> Integer
deviceSize = pdevice_size . toPseudoDevice

toDevice :: Tool -> Maybe Device
toDevice (DeviceTool _ d) = Just d

toolName :: Tool -> String
toolName (DeviceTool _ d) = device_name d
