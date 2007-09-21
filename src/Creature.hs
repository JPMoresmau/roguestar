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
     dbWalkCreature)
    where

import Data.Maybe
import Control.Monad.State
import CreatureData
import DB
import SpeciesData
import Species
import Tests
import DBData
import Dice
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
       random_id <- 1 `d` 2000
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
dbNewCreature :: Faction -> Species -> DB CreatureRef
dbNewCreature faction species = 
    do dbAddCreature =<< dbGenerateCreature faction species

-- |
-- Causes the creature to walk in the specified facing direction.
dbWalkCreature :: Facing -> CreatureRef -> DB ()
dbWalkCreature facing creature_ref =
    do dbTurnCreature facing creature_ref
       loc <- liftM (snd . fromJust) $ dbWhere creature_ref
       let loc' = case loc of
                      DBCoordinateLocation (x,y) ->
                          DBCoordinateLocation ((x+delta_x,y+delta_y))
                      DBCoordinateFacingLocation ((x,y),old_facing) -> 
                          DBCoordinateFacingLocation ((x+delta_x,y+delta_y),old_facing)
           (delta_x,delta_y) = facingToRelative facing
--           movement_cost = case (abs delta_x,abs delta_y) of
--                               (0,0) -> 0%1
--                               (1,0) -> x%1
--                               (0,1) -> y%1
--                               (1,1) -> 7%5
--                               _ -> error "dbWalkCreature: facingToRelative should only answer in the range -1..1
       dbMoveTo creature_ref loc'

dbTurnCreature :: Facing -> CreatureRef -> DB ()
dbTurnCreature facing creature_ref =
    do loc <- liftM (snd . fromJust) $ dbWhere creature_ref
       let loc' = case loc of
                          DBCoordinateLocation xy ->
                              DBCoordinateFacingLocation (xy,facing)
                          DBCoordinateFacingLocation (xy,_) ->
                              DBCoordinateFacingLocation (xy,facing)
--         movement_cost = facingDistance old_facing facing % 4
       dbMoveTo creature_ref loc'

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
