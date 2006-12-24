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

module Main (main)
    where

import DB
import System
import System.Random
import List
import Tests
import Dice
import Creature
import InsidenessMap
import Control.Monad
import TerrainData
import HopList
import Protocol
import GridRayCaster

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
						   hopListTests ++
						   gridRayCasterTests)
		       if testsPassed
			  then putStrLn "All tests passed."
			  else putStrLn "Error: a test failed."

runByArgs "version" = do putStrLn program_id_string

runByArgs "test-character-generator" = do runCreatureGenerationTest

runByArgs "test-terrain-generator" = do seed <- randomIO
					let example_terrain = generateExampleTerrain seed
					    in do putStrLn "Terrain Map of (-20..20),(-10..10)"
                                                  mapM_ putStrLn $ prettyPrintTerrain ((-20,20),(-10,10)) example_terrain
						  putStrLn "Terrain Map of (5460..5500),(-1010..-990)"
                                                  mapM_ putStrLn $ prettyPrintTerrain ((5460,5500),(-1010,-990)) example_terrain
						  putStrLn "Terrain Map of (5461..5501),(-1009..-989)"
                                                  mapM_ putStrLn $ prettyPrintTerrain ((5461,5501),(-1009,-989)) example_terrain

runByArgs "begin" = do db0 <- initialDB
		       mainLoop db0

runByArgs "over" = putStrLn "over"

runByArgs "help" = do putStrLn "Commands:"
		      putStrLn "begin                    - begin a protocol session (used by GUI clients and experts)"
		      putStrLn "help                     - print this message"
		      putStrLn "over                     - print \"over\" on a line by itself"
		      putStrLn "tests                    - run unit tests"
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