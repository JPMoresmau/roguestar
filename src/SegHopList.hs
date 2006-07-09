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

module SegHopList
    (SegHopList,SegHopList.fromList,SegHopList.index)
    where

import SegmentList
import HopList
import Data.Array

-- |
-- A system that combines the benefits of the SegmentList and the HopList
-- to access data arbitrarily far away in an infinite list quickly.
--
type SegHopList a = HopList (Array Int a)

fromList :: [a] -> SegHopList a
fromList xs = HopList.fromList (segmentList xs)

index :: SegHopList a -> Integer -> a
index shl i = (shl `HopList.index` (i `div` segmentSizeI)) ! ((fromInteger i) `mod` segmentSizei)
