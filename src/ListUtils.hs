--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
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

module ListUtils
    (doubles,
     loopedDoubles,
     consecutives,
     loopedConsecutives)
    where

-- |
-- Converts a list of length two into a list of tuple pairs.
--
pairify :: [a] -> (a,a)
pairify (m:n:[]) = (m,n)
pairify _ = error "pairify only works on lists of length two"

-- |
-- Effectively consecutives 2
--
doubles :: [a] -> [(a,a)]
doubles = (map pairify) . (consecutives 2)

loopedDoubles :: [a] -> [(a,a)]
loopedDoubles = (map pairify) . (loopedConsecutives 2)

-- |
-- Answers a list containing every sequence of n consecutive
-- elements in the parameter, non-circular.
--
-- consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4]]
--
consecutives :: Int -> [a] -> [[a]]
consecutives n elems = let taken = take n elems
			   in if (length taken == n)
			      then (taken : (consecutives n $ tail elems))
			      else []

-- |
-- Answers a list containing every sequence of n consecutive
-- elements in the parameter, in a circular way so that the first
-- element of the list is considered subsequent to the first.
--
-- consecutives 3 [1,2,3,4] = [[1,2,3],[2,3,4],[3,4,1],[4,1,2]]
--
loopedConsecutives :: Int -> [a] -> [[a]]
loopedConsecutives n elems = consecutives n $ take (n + length elems - 1) $ cycle elems
