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
    percentAttribute,
    multipleAttribute)
    where

import Data.List

-- |
-- Used to randomly generate attributes for an entity.
-- AttributeAlways is a generator that always creates the specified attribute.
-- (AttributeSometimes attrib x $ otherwise) is a generator that generates
-- the the attribute "attrib" x-fraction of the time, and invokes the attribute
-- generator "otherwise" otherwise.
--

data AttributeGenerator a = AttributeAlways a
                          | AttributeSometimes a Rational (Maybe (AttributeGenerator a))
                            deriving (Show, Read)

-- |
-- Grants the entity the specified attribute x percent of the time, otherwise nothing
--
percentAttribute :: a -> Rational -> AttributeGenerator a
percentAttribute attr x = AttributeSometimes attr x $ Nothing

-- |
-- Grants the entity the specified attribute between minimum and maximum instances of the
-- attribute, on average the average of the two (as a binomial distribution).
--
multipleAttribute :: a -> (Integer,Integer) -> [AttributeGenerator a]
multipleAttribute attr (mini,maxi) | mini >= 0 && maxi >= mini = 
    (genericReplicate mini $ AttributeAlways attr) ++ (genericReplicate (maxi-mini) $ percentAttribute attr 50)
multipleAttribute _ _ = error "multipleAttribute: maximum < minimum badness"