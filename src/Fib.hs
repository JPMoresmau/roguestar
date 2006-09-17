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

module Fib
    (fib,fibList)
    where

import Data.List

-- |
-- The fibonacci sequence beginning with the two specified
-- starting integers.  The most famous fibonacci sequence is
-- genericFib (1,1) = [2,3,5,8,13,21 . . .]
-- The result does not include the leading [1,1].
--
generalFib :: (Integer,Integer) -> [Integer]
generalFib (minusTwo,minusOne) = let zero = minusTwo + minusOne
				     in zero:(generalFib (minusOne,zero))

-- |
-- genericFib with leading two integers.
--
generalFib' :: (Integer,Integer) -> [Integer]
generalFib' (a,b) = a:b:(generalFib (a,b))

-- |
-- (fib n) answers the nth element of the fibonacci sequence.
--
fib :: Integer -> Integer
fib = genericIndex fibList

-- |
-- The fibonacci sequence as a haskell list [1,1,2,3,5,8 . . .].
--
fibList :: [Integer]
fibList = generalFib' (1,1)