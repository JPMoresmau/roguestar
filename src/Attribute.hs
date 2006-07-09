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

module Attribute
    (generateAttributes)
    where

import AttributeData
import DB
import Dice
import Data.Maybe

--
-- Randomly generate 1 attribute from an attribute generator.
--
generate1Attribute :: AttributeGenerator a -> DB (Maybe a)
generate1Attribute (AttributeAlways someAttrib) = do return (Just someAttrib)
generate1Attribute (AttributeSometimes someAttrib percentage_chance maybeNextGen) = do percentage_roll <- (1 `d` 100)
										       if (percentage_roll <= percentage_chance)
											  then return (Just someAttrib)
											  else case maybeNextGen of
														 Just nextGen -> generate1Attribute nextGen
														 Nothing -> return Nothing

-- 
-- Randomly generate attributes from a list of AttributeGenerators.
--
generateAttributes :: [AttributeGenerator a] -> DB [a]
generateAttributes attribGens = do maybeAttribs <- mapM generate1Attribute attribGens
				   return $ map fromJust $ filter isJust maybeAttribs