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

module CharacterData
    (CharacterClass(..),
     all_character_classes,
     base_character_classes)
    where

data CharacterClass = Barbarian
		    | Consular
		    | Engineer
		    | ForceAdept
		    | Marine
		    | Ninja
		    | Pilot
		    | Privateer
		    | Scout
		    | Shepherd
		    | Thief
		    | Warrior
		    deriving (Eq,Enum,Bounded,Read,Show)

all_character_classes :: [CharacterClass]
all_character_classes = [minBound..maxBound]

base_character_classes :: [CharacterClass]
base_character_classes = [Barbarian,
			  Consular,
			  Engineer,
			  ForceAdept,
			  Marine,
			  Ninja,
			  Pilot,
			  Privateer,
			  Scout,
			  Shepherd,
			  Thief,
			  Warrior]
