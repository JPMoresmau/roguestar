module Main (main)
    where

import System
import List
import Maybe
import Tests
import Dice
import Species
import Creature
import PeriodicTable
import Control.Monad

program_version_number = "0.0.1"
program_name = "roguestar-engine"
program_variant = "standard"

runByArgs :: String -> IO ()

runByArgs "tests" = do testsPassed <- runAllTests ([sampleTestCase] ++ testDice ++ creatureTests)
		       if testsPassed
			  then putStrLn "All tests passed."
			  else putStrLn "Error: a test failed."

runByArgs "version" = do putStrLn (program_name ++ " " ++ program_version_number ++ " (" ++ program_variant ++ ")")

runByArgs "test-character-generator" = do runCreatureGenerationTest

runByArgs "periodic-table" = do displayPeriodicTable

runByArgs "help" = do putStrLn "Commands:"
		      putStrLn "help - print this message"
		      putStrLn "periodic-table - print the roguestar periodic table of the elements"
		      putStrLn "tests - run most unit tests"
		      putStrLn "test-character-generator - generate a random sample creature"
		      putStrLn "version - print the version string"

runByArgs invalidArgument = do putStrLn ("Error: unrecognized argument: " ++ invalidArgument)

--
-- Each argument corresponds to a particular "runByArgs" command.  Run them all in order.
--
main :: IO ()
main = do args <- getArgs
	  mapM_ runByArgs args