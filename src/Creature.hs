module Creature 
    (runCreatureGenerationTest, creatureTests)
    where

import Control.Monad.State
import CreatureData
import DB
import SpeciesData
import Species
import Tests

runCreatureGenerationTest :: IO ()
runCreatureGenerationTest = do db0 <- initial_db
			       putStrLn $ show $ evalState (generateCreatureData exampleSpecies) db0

-- |
-- Generates a new random creature from the specified species.
--
newCreature :: Species -> DB Creature
newCreature species = do (stats,attribs,name) <- generateCreatureData species
			 return  (Creature { creature_stats=stats,
					     creature_attribs=attribs,
					     creature_name=name,
					     creature_damage=0 })

-- |
-- Adds a creature to the DB.
--
dbNewCreature :: Species -> DB ObjectRef
dbNewCreature species = do newc <- newCreature species
			   dbAddObjectThing (CreatureRef) (CreatureThing newc)

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
