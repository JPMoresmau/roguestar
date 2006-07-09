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

module Dice (testDice, d)
    where

import Control.Monad.State
import DB
import Tests

--
-- Answers a result of rolling n s-sided dice, numbered 1 to s.
--
d :: Integer -> Integer -> DB Integer
d 0 _ = do return 0
d 1 s | (s > 0)           = do roll <- dbNextRandomInteger
	                       return ((roll `mod` s) + 1)
d n s | (n >= 2 && s > 0) = do roll <- d 1 s
		               rolls <- d (n-1) s
		               return (roll + rolls)
d _ _ = error "Rolled negative dice or negative sided dice."

--
-- Test one die roll.  This does not test that die rolls are "sufficiently" random,
-- only the results fall within possible ranges.
--
testDieRoll :: Integer -> Integer -> TestCase
testDieRoll n s = do db0 <- initial_db
		     let dieRoll = evalState (n `d` s) db0
			 testStr = ("testDice " ++ show n ++ "d" ++ show s ++ "(" ++ show dieRoll ++ ")")
			 in return (if dieRoll >= n && dieRoll <= n*s
				    then Passed testStr
				    else Failed testStr)

--
-- Several dice related test cases.
--
testDice_ :: [TestCase]
testDice_ = 
    [ testDieRoll 1 10,
      testDieRoll 2 5,
      testDieRoll 0 3,
      testDieRoll 15 23 ]

testDice :: [TestCase]
testDice = foldr1 (++) (replicate 4 testDice_)