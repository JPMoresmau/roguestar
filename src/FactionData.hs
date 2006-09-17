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

module FactionData
    (Faction(..))
    where

data Faction = Player
	     | InterstellarConcordance        -- the lawful galactic government
	     | PanGalacticTreatyOrganization  -- the neutral galactic government
	     | ImperialAlliance               -- the chaotic galactic government
	     | MonstersInc                    -- nonsentient monsters
	     | Nonaligned                     -- pirates, mecenaries, your friendly neighborhood police office, etc 
	     | Cyborgs                        -- cyborgs
	     | Whispers                       -- the dark indifferent destroyers of worlds
	     | Proselytes                     -- evil entities that possess others' minds
	     | Civilian                       -- merchants, children -- don't kill these
	       deriving (Eq,Read,Show)