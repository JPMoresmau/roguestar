{-# LANGUAGE OverloadedStrings #-}
module ToolData 
    (Tool(..),
     fromSphere,
     sphere,
     Device,
     PseudoDevice(..),
     DeviceKind(..),
     DeviceFunction(..),
     DeviceType(..),
     deviceName,
     deviceOutput,
     deviceAccuracy,
     deviceSpeed,
     deviceDurability,
     deviceSize,
     deviceChromalite,
     deviceMaterial,
     deviceGas,
     improvised,
     phase_pistol,
     phaser,
     phase_rifle,
     kinetic_fleuret,
     kinetic_sabre)
    where

import Substances
import qualified Data.ByteString.Char8 as B

data Tool = DeviceTool DeviceFunction Device
          | Sphere Substance
    deriving (Read,Show,Eq)

-- | Get the substance type of a material sphere, if it is one.
fromSphere :: Tool -> Maybe Substance
fromSphere (Sphere s) = Just s
fromSphere _ = Nothing

sphere :: (SubstanceType a) => a -> Tool
sphere = Sphere . toSubstance

data DeviceFunction = Gun | Sword
            deriving (Read,Show,Eq)

data DeviceKind =
    Pistol
  | Carbine
  | Rifle
  | Fleuret
  | Sabre
        deriving (Read,Show,Eq)

kindToFunction :: DeviceKind -> (DeviceFunction,Integer)
kindToFunction Pistol = (Gun,1)
kindToFunction Carbine = (Gun,3)
kindToFunction Rifle = (Gun,5)
kindToFunction Fleuret = (Sword,2)
kindToFunction Sabre = (Sword,4) 

-- | Any kind of device that is constructed from a power cell, materal, and gas medium,
-- using the various device rules to determine it's power.
data Device = Device {
   device_name :: B.ByteString,
   device_chromalite :: Chromalite,
   device_material :: Material,
   device_gas :: Gas,
   device_size :: Integer }
     deriving (Eq,Read,Show)

-- | Anything that operates like a device, but isn't.  For example, an unarmed attack.
data PseudoDevice = PseudoDevice {
    pdevice_accuracy :: Integer,
    pdevice_output :: Integer,
    pdevice_speed :: Integer,
    pdevice_size :: Integer }

class DeviceType d where
    toPseudoDevice :: d -> PseudoDevice

instance DeviceType Device where
    toPseudoDevice d = let chromalite = chromalitePotency $ device_chromalite d
                           gas = gasValue $ device_gas d
                           material = material_critical_value $ materialValue $ device_material d
                           size = device_size d
        in PseudoDevice {
               pdevice_accuracy = min chromalite material + chromalite,
               pdevice_output = min chromalite gas + chromalite,
               pdevice_speed = gas + material,
               pdevice_size = size }

instance DeviceType PseudoDevice where
    toPseudoDevice = id

device :: B.ByteString -> DeviceKind -> Chromalite -> Material -> Gas -> Tool
device s dk c m g = DeviceTool func (Device s c m g size)
    where (func,size) = kindToFunction dk

improvised :: DeviceKind -> Chromalite -> Material -> Gas -> Tool
improvised dk c m g = device ("improvised_" `B.append` B.pack (show dk)) dk c m g

phase_pistol :: Tool
phase_pistol = device "phase_pistol" Pistol Caerulite Zinc Flourine

phaser :: Tool
phaser = device "phaser" Carbine Caerulite Zinc Flourine

phase_rifle :: Tool
phase_rifle = device "phase_rifle" Rifle Caerulite Zinc Flourine

kinetic_fleuret :: Tool
kinetic_fleuret = device "kinetic_fleuret" Fleuret Ionidium Aluminum Nitrogen

kinetic_sabre :: Tool
kinetic_sabre = device "kinetic_sabre" Sabre Ionidium Aluminum Nitrogen

deviceName :: Device -> B.ByteString
deviceName = device_name

deviceDurability :: Device -> Integer
deviceDurability d = device_size d * (material_construction_value $ materialValue $ device_material d)

deviceOutput :: (DeviceType d) => d -> Integer
deviceOutput = pdevice_output . toPseudoDevice

deviceAccuracy :: (DeviceType d) => d -> Integer
deviceAccuracy = pdevice_accuracy . toPseudoDevice

deviceSpeed :: (DeviceType d) => d -> Integer
deviceSpeed = pdevice_speed . toPseudoDevice

deviceSize :: (DeviceType d) => d -> Integer
deviceSize = pdevice_size . toPseudoDevice

deviceChromalite :: Device -> Chromalite
deviceChromalite = device_chromalite

deviceMaterial :: Device -> Material
deviceMaterial = device_material

deviceGas :: Device -> Gas
deviceGas = device_gas
