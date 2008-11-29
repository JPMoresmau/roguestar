module ToolData 
    (Tool(..),
     Gun,
     gunEnergyOutput,
     gunThroughput,
     gunEndurance,
     toolName,
     phase_pistol,
     phaser,
     phase_rifle)
    where

import Substances

data Tool = GunTool Gun
            deriving (Read,Show,Eq)

data GunSize = Pistol
             | Carbine
             | Rifle
	     | Cannon
	     | Launcher
             deriving (Read,Show,Eq)

data Gun = Gun {
   gun_name :: String,
   gun_power_cell :: Chromalite,
   gun_substrate :: Material,
   gun_medium :: Gas,
   gun_size :: GunSize }
     deriving (Eq,Read,Show)

phase_pistol :: Tool
phase_pistol = GunTool $ Gun "phase_pistol" Pteulanium Zinc Argon Pistol

phaser :: Tool
phaser = GunTool $ Gun "phaser" Pteulanium Zinc Argon Carbine

phase_rifle :: Tool
phase_rifle = GunTool $ Gun "phase_rifle" Pteulanium Zinc Argon Rifle

gunEnergyOutput :: Gun -> Integer
gunEnergyOutput g = gunSizeClass g * (chromalitePotency $ gun_power_cell g)

gunThroughput :: Gun -> Integer
gunThroughput g = ((material_critical_value $ materialValue $ gun_substrate g) + 1) *
                  (gasWeight $ gun_medium g)

gunEndurance :: Gun -> Integer
gunEndurance g = 10 * (material_construction_value $ materialValue $ gun_substrate g)^2

gunSizeClass :: Gun -> Integer
gunSizeClass (Gun { gun_size = Pistol }) = 1
gunSizeClass (Gun { gun_size = Carbine}) = 3
gunSizeClass (Gun { gun_size = Rifle}) = 5
gunSizeClass (Gun { gun_size = Cannon}) = 7
gunSizeClass (Gun { gun_size = Launcher}) = 10

toolName :: Tool -> String
toolName (GunTool (Gun { gun_name = s })) = s
