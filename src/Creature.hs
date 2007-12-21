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
     dbNewCreature,
     dbTurnCreature,
     dbStepCreature)
    where

import Control.Monad.State
import Data.Maybe
import CreatureData
import DB
import SpeciesData
import Species
import Tests
import DBData
import FactionData
import Facing

runCreatureGenerationTest :: IO ()
runCreatureGenerationTest = do db0 <- initialDB
			       putStrLn $ show $ evalState (generateCreatureData exampleSpecies) db0

-- |
-- Generates a new Creature from the specified species.
--
dbGenerateCreature :: Faction -> Species -> DB Creature
dbGenerateCreature faction species = 
    do (stats,attribs,name) <- generateCreatureData species
       random_id <- dbNextRandomInteger
       return (Creature { creature_stats=stats,
			  creature_attribs=attribs,
			  creature_species_name=name,
			  creature_random_id=random_id,
			  creature_damage=0,
			  creature_faction=faction})

-- |
-- During DBRaceSelectionState, generates a new Creature for the player character and sets it into the 
-- database's DBClassSelectionState.
--
dbGenerateInitialPlayerCreature :: Species -> DB ()
dbGenerateInitialPlayerCreature species = 
    do newc <- dbGenerateCreature Player species
       dbSetStartingRace species
       dbSetState (DBClassSelectionState newc)

-- |
-- Generates a new Creature from the specified Species and adds it to the database.
--
dbNewCreature :: (CreatureLocation l) => Faction -> Species -> l -> DB CreatureRef
dbNewCreature faction species loc = 
    do creature <- dbGenerateCreature faction species
       dbAddCreature creature loc

-- |
-- Causes the creature to walk in the specified facing direction.
--
dbWalkCreature :: Facing -> (Integer,Integer) -> CreatureRef -> DB ()
dbWalkCreature face (x',y') creature_ref =
    do dbMove creature_ref $ \l -> return $ fromMaybe l $
          do p <- liftM location $ toPlanarLocation l
             Position (x,y) <- liftM location $ toPositionLocation l
             return $ genericLocationP $
                          standCreature l (Standing { standing_plane = p,
                                                      standing_position = Position (x+x',y+y'),
                                                      standing_facing = face })
       return ()

dbStepCreature :: Facing -> CreatureRef -> DB ()
dbStepCreature face = dbWalkCreature face (facingToRelative face)

dbTurnCreature :: Facing -> CreatureRef -> DB ()
dbTurnCreature face = dbWalkCreature face (0,0)

creatureTests :: [TestCase]
creatureTests = [testHitPointCalculation,testAlive,testDead,
                 testEffectiveLevel,testMeleeAttackBonus]

testHitPointCalculation :: TestCase
testHitPointCalculation = if (creatureScore MaxHitPoints exampleCreature1 == 33)
			  then return (Passed "testHitPointCalculation")
			  else return (Failed ("testHitPointCalculation" ++ "(" ++ (show (creatureScore MaxHitPoints exampleCreature1)) ++ ")"))

testAlive :: TestCase
testAlive = if (alive $ injure 34 exampleCreature1)
	    then return (Passed "testAlive")
	    else return (Failed "testAlive")

testDead :: TestCase
testDead = if (dead $ injure 36 exampleCreature1)
           then return (Passed "testDead")
	   else return (Failed "testDead")

testEffectiveLevel :: TestCase
testEffectiveLevel = let effective_level = (creatureScore EffectiveLevel exampleCreature1)
                         result_string = "testEffectiveLevel: (" ++ (show effective_level) ++ ")"
                         in if effective_level == 14
                            then return (Passed result_string)
                            else return (Failed result_string)

testMeleeAttackBonus :: TestCase
testMeleeAttackBonus = let bonus = (creatureScore MeleeAttack exampleCreature1)
                           result_string = "testMeleeAttackBonus: (" ++ (show bonus) ++ ")"
                           in if bonus == 23
                              then return (Passed result_string)
                              else return (Failed result_string)
