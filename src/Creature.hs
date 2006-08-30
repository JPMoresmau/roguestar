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

module Creature 
    (dbGenerateInitialPlayerCreature,
     runCreatureGenerationTest, 
     creatureTests,
     dbNewCreature)
    where

import Control.Monad.State
import CreatureData
import DB
import SpeciesData
import Species
import Tests
import DBData

runCreatureGenerationTest :: IO ()
runCreatureGenerationTest = do db0 <- initialDB
			       putStrLn $ show $ evalState (generateCreatureData exampleSpecies) db0

-- |
-- Generates a new Creature from the specified species.
--
newCreature :: Species -> DB Creature
newCreature species = do (stats,attribs,name) <- generateCreatureData species
			 return  (Creature { creature_stats=stats,
					     creature_attribs=attribs,
					     creature_name=name,
					     creature_damage=0 })

-- |
-- During DBRaceSelectionState, generates a new Creature for the player character and sets it into the 
-- database's DBClassSelectionState.
--
dbGenerateInitialPlayerCreature :: Species -> DB ()
dbGenerateInitialPlayerCreature species = do newc <- newCreature species
					     dbSetStartingRace species
					     dbSetState (DBClassSelectionState newc)

-- |
-- Generates a new Creature from the specified Species and adds it to the database.
--
dbNewCreature :: Species -> DB CreatureRef
dbNewCreature species = do newc <- newCreature species
			   dbAddCreature newc

creatureTests :: [TestCase]
creatureTests = [testHitPointCalculation,testAlive,testDead,
                 testEffectiveLevel,testMeleeAttackBonus]

testHitPointCalculation :: TestCase
testHitPointCalculation = do if (maxHitPoints exampleCreature1 == 23)
				then return (Passed "testHitPointCalculation")
				else return (Failed ("testHitPointCalculation" ++ "(" ++ (show (maxHitPoints exampleCreature1)) ++ ")"))

testAlive :: TestCase
testAlive = do if (alive $ injure 24 exampleCreature1)
		  then return (Passed "testAlive")
		  else return (Failed "testAlive")

testDead :: TestCase
testDead = do if (dead $ injure 26 exampleCreature1)
                 then return (Passed "testDead")
		 else return (Failed "testDead")

testEffectiveLevel :: TestCase
testEffectiveLevel = let effective_level = (creatureEffectiveLevel exampleCreature1)
                         result_string = "testEffectiveLevel: (" ++ (show effective_level) ++ ")"
                         in if effective_level == 13
                            then return (Passed result_string)
                            else return (Failed result_string)

testMeleeAttackBonus :: TestCase
testMeleeAttackBonus = let bonus = (creatureMeleeAttackBonus exampleCreature1)
                           result_string = "testMeleeAttackBonus: (" ++ (show bonus) ++ ")"
                           in if bonus == 2
                              then return (Passed result_string)
                              else return (Failed result_string)
