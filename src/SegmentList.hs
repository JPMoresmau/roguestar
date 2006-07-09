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

module SegmentList
    (segmentSizei,segmentSizeI,segmentList,segmentIndex)
    where

import Data.List
import Data.Array

segmentSizei :: Int
segmentSizei = 100

segmentSizeI :: Integer
segmentSizeI = toInteger segmentSizei

-- |
-- Constructs a list in which chunks of sequential elements are held together
-- in an array, to improve access time.  This is only intended for
-- use in an infinite list (otherwise just pack the entire thing
-- in one array).
-- 
segmentList :: [a] -> [Array Int a]
segmentList xs = let (firstGroup,restGroups) = seqSplitAt segmentSizei xs
		     in (listArray (0,segmentSizei-1) firstGroup) :
			    (segmentList restGroups)

seqSplitAt :: Int -> [a] -> ([a],[a])
seqSplitAt 0 xs = ([],xs)
seqSplitAt i (x:xs) = let rest = (seqSplitAt (i-1) xs)
			in seq x $ (x : (fst rest),snd rest)
seqSplitAt i [] = error ("Tried to access " ++ (show i) ++ "'th element of []")

-- |
-- Retrieve an element from a segment list by index.
--
segmentIndex :: [Array Int a] -> Integer -> a
segmentIndex xss i = (xss `genericIndex` (i `div` segmentSizeI)) ! ((fromInteger i) `mod` segmentSizei)