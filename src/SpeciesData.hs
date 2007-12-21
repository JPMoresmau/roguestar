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

module SpeciesData
    (male,
     female,
     exampleSpecies,
     Species(..),
     CreatureGenerationData)
    where

import StatsData
import CreatureData
import AttributeData

--
-- Makes the creature male x percent of the time (female otherwise).
--
male :: Rational -> AttributeGenerator CreatureAttribute
male x = AttributeSometimes (Gender Male) x $ Just (AttributeAlways (Gender Female))

--
-- Makes the creature female x percent of the time (male otherwise).
--
female :: Rational -> AttributeGenerator CreatureAttribute
female x = AttributeSometimes (Gender Female) x $ Just (AttributeAlways (Gender Male))

data Species = Species { averages :: Stats, 
                         distributions :: Stats,
                         attribute_generator :: [AttributeGenerator CreatureAttribute],
                         species_name :: String } 
	       deriving (Show, Read)

--
-- Tuple that contains generated data for a new creature.  Contains the stats for the new creature,
-- the attributes, and the name of the creature's species.
--
type CreatureGenerationData = ( Stats, [CreatureAttribute], String )

--
-- An example species.
--
exampleSpecies :: Species
exampleSpecies = Species {
			  averages = Stats { strength=1, dexterity=(-2), constitution=1, intelligence=(-1), perception=(-1), charisma=3, mindfulness=(-1) },
			  distributions = (stats 2),
			  attribute_generator = [male 0.4],
			  species_name = "Example-Species" 
			 }
