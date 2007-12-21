--------------------------------------------------------------------------
--  roguestar-engine: the space-adventure roleplaying game backend.       
--  Copyright (C) 2006 Christopher Lane Hinson <lane@downstairspeople.org>  
--                                                                        
--  This program is free software; you can redistribute it and/or modify  
--  it under the terms of the GNU General Public License as published by  
--  the Free Software Foundation; either version 2 of the License, or     
--  (at your option) any later version.                                   
--                                                                        
--  This program is distributed in the hope that it will be useful,       
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
--  GNU General Public License for more details.                          
--                                                                        
--  You should have received a copy of the GNU General Public License along  
--  with this program; if not, write to the Free Software Foundation, Inc.,  
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.           
--                                                                        
--------------------------------------------------------------------------

module BeginGame
    (dbBeginGame)
    where

import Plane
import CreatureData
import Character
import CharacterData
import DB
import DBData
import Facing
import TerrainData
import Data.Maybe
import ToolData

player_race_to_biome :: [(String,Biome)]
player_race_to_biome =
    [("anachronid",DesertBiome),
     ("androsynth",RockBiome),
     ("ascendant",MountainBiome),
     ("canduceator",SwampBiome),
     ("encephalon",SwampBiome),
     ("goliath",DesertBiome),
     ("hellion",GrasslandBiome),
     ("kraken",OceanBiome),
     ("myrmidon",DesertBiome),
     ("perennial",ForestBiome),
     ("recreant",DesertBiome),
     ("reptilian",SwampBiome)]

dbCreateStartingPlane :: Creature -> DB PlaneRef
dbCreateStartingPlane creature =
    dbNewPlane $ TerrainGenerationData {tg_smootheness = 3,
					tg_biome = fromMaybe GrasslandBiome $ lookup (creature_species_name creature) player_race_to_biome}

-- |
-- Begins the game with the specified starting player creature and the specified starting character class.
-- The character class should not be pre-applied to the creature.
--
dbBeginGame :: Creature -> CharacterClass -> DB ()
dbBeginGame creature character_class = 
    do let first_level_creature = applyCharacterClass character_class creature
       plane_ref <- dbCreateStartingPlane creature
       landing_site <- pickRandomClearSite 200 30 2 plane_ref
       creature_ref <- dbAddCreature first_level_creature (Standing plane_ref landing_site Here)
       phaser_position <- pickRandomClearSite 200 30 2 plane_ref
       dbAddTool phase_pistol (Dropped plane_ref phaser_position)
       dbSetState $ DBPlayerCreatureTurn creature_ref