module ToolData 
    (Tool(..),
     Device,
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

data Tool = DeviceTool DeviceType Device
    deriving (Read,Show,Eq)

data DeviceType = Gun | Sword
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

deviceEnergyOutput :: Device -> Integer
deviceEnergyOutput g = device_size g * (chromalitePotency $ device_power_cell g)

deviceThroughput :: Device -> Integer
deviceThroughput g = ((material_critical_value $ materialValue $ device_material g) + 1) *
                     (gasWeight $ device_medium g)

deviceDurability :: Device -> Integer
deviceDurability g = device_size g * (material_construction_value $ materialValue $ device_material g)

deviceSize :: Device -> Integer
deviceSize = device_size

toDevice :: Tool -> Maybe Device
toDevice (DeviceTool _ d) = Just d

toolName :: Tool -> String
toolName (DeviceTool _ d) = device_name d
