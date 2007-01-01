module ToolData 
    (Tool(..),
     GunType(..),
     toolName,
     phase_pistol,
     phaser,
     phase_rifle)
    where
    
import Substances
    
data Tool = Gun { gun_power_cell :: Chromalite,
                  gun_type :: GunType }
            deriving (Read,Show)
                   
data GunType = Pistol   -- itty bitty
             | Carbine  -- moderate size
             | Rifle    -- big
             deriving (Read,Show)
             
gun :: GunType -> Chromalite -> Tool
gun t c = Gun { gun_power_cell = c,
                gun_type = t }
      
phase_pistol :: Tool
phase_pistol = gun Pistol Pteulanium
             
phaser :: Tool
phaser = gun Carbine Pteulanium

phase_rifle :: Tool
phase_rifle = gun Rifle Pteulanium

toolName :: Tool -> String
toolName (Gun { gun_power_cell = Pteulanium, gun_type = Pistol }) = "phase_pistol"
toolName (Gun { gun_power_cell = Pteulanium, gun_type = Carbine }) = "phaser"
toolName (Gun { gun_power_cell = Pteulanium, gun_type = Rifle }) = "phase_rifle"
toolName (Gun {}) = "unknown_gun"