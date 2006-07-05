module Main (main)
    where

import System
import System.Random
import List
import Tests
import Dice
import Creature
import InsidenessMap
import PeriodicTable
import Control.Monad
import TerrainData
import HopList

program_version_number :: String
program_version_number = "0.0.1"

program_name :: String
program_name = "roguestar-engine"

program_variant :: String
program_variant = "standard"

program_id_string :: String
program_id_string = (program_name ++ " " ++ program_version_number ++ " (" ++ program_variant ++ ")")

-- |
-- Processes a single command line argument.
--
runByArgs :: String -> IO ()

runByArgs "tests" = do testsPassed <- runAllTests ([sampleTestCase] ++ 
						   testDice ++ 
						   creatureTests ++
						   insidenessTests ++
						   hopListTests)
		       if testsPassed
			  then putStrLn "All tests passed."
			  else putStrLn "Error: a test failed."

runByArgs "version" = do putStrLn program_id_string

runByArgs "test-character-generator" = do runCreatureGenerationTest

runByArgs "test-terrain-generator" = do seed <- randomIO
					let example_terrain = generateExampleTerrain seed
					    in do mapM_ putStrLn $ prettyPrintTerrain ((460,500),(-1010,-990)) example_terrain
						  mapM_ putStrLn $ prettyPrintTerrain ((461,501),(-1009,-989)) example_terrain

runByArgs "periodic-table" = do displayPeriodicTable

runByArgs "help" = do putStrLn "Commands:"
		      putStrLn "help                     - print this message"
		      putStrLn "periodic-table           - print the roguestar periodic table of the elements"
		      putStrLn "tests                    - run most unit tests"
		      putStrLn "test-character-generator - generate a random sample creature"
		      putStrLn "test-terrain-generator   - display an example terrain map"
		      putStrLn "version                  - print the version string"

runByArgs invalidArgument = do putStrLn ("Error: unrecognized argument: " ++ invalidArgument)
			       fail "Unrecognized argument in runByArgs"

--
-- Each argument corresponds to a particular "runByArgs" command.  Run them all in order.
--
main :: IO ()
main = do args <- getArgs
	  mapM_ runByArgs args