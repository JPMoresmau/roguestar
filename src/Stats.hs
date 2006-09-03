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

module Stats (generateStats) 
    where

import Dice
import StatsData
import DB

--
-- Randomly generate 1 statistic.
--
generate1Stat :: Integer -> Integer -> DB Integer
generate1Stat average deviation = do dieRoll <- (deviation `d` 3)
				     return (dieRoll + average - 2*deviation)

--
-- Randomly generate statistics.
--
generateStats :: Stats -> Stats -> DB Stats
generateStats averages deviations = do new_str <- generate1Stat (str averages) (str deviations)
				       new_dex <- generate1Stat (dex averages) (dex deviations)
				       new_con <- generate1Stat (con averages) (con deviations)
				       new_int <- generate1Stat (int averages) (int deviations)
				       new_per <- generate1Stat (per averages) (per deviations)
				       new_cha <- generate1Stat (cha averages) (cha deviations)
				       new_mind <- generate1Stat (mind averages) (mind deviations)
				       return Stats { strength = new_str,
						      dexterity = new_dex,
						      constitution = new_con,
						      intelligence = new_int,
						      perception = new_per,
						      charisma = new_cha,
						      mindfulness = new_mind }