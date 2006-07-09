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

module AttributeData
    (AttributeGenerator(..),
    percent_attribute)
    where

--
-- Used to randomly generate attributes for an entity.
-- AttributeAlways is a generator that always creates the specified attribute.
-- (AttributeSometimes attrib x $ otherwise) is a generator that generates
-- the the attribute "attrib" x-percent of the time, and invokes the attribute
-- generator "otherwise" otherwise.
--

data AttributeGenerator a = AttributeAlways a
                          | AttributeSometimes a Integer (Maybe (AttributeGenerator a))
                            deriving (Show, Read)

--
-- Grants the creature the specified attribute x percent of the time, otherwise nothing
--
percent_attribute :: a -> Integer -> AttributeGenerator a
percent_attribute attr x = AttributeSometimes attr x $ Nothing