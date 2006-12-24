module ToolData 
    (Tool(..),
     GunType(..),
     gun,
     taser,
     phaser,
     phase_rifle)
    where
    
import Substances
    
data Tool = Gun { gun_power_cell :: Chromalite,
                  gun_shots_remaining :: Integer,
                  gun_type :: GunType }
            deriving (Read,Show)
                   
data GunType = Pistol   -- itty bitty
             | Carbine  -- moderate size
             | Rifle    -- big
             deriving (Read,Show)
             
gunTypeShotsMultiplier :: GunType -> Integer
gunTypeShotsMultiplier Pistol = 4
gunTypeShotsMultiplier Carbine = 2
gunTypeShotsMultiplier Rifle = 1
             
gun :: GunType -> Chromalite -> Tool
gun t c = Gun { gun_power_cell = c,
                gun_shots_remaining = gunTypeShotsMultiplier t * chromalitePotency c,
                gun_type = t }
      
taser :: Tool
taser = gun Pistol Pteulanium
             
phaser :: Tool
phaser = gun Carbine Pteulanium

phase_rifle :: Tool
phase_rifle = gun Rifle Pteulanium