
module Main (main)
    where

import DB
import System.Environment
import Tests
import HierarchicalDatabase
import TerrainData
import Protocol
import GridRayCaster
import Data.Version
import Paths_roguestar_engine
import Data.List (intersperse)
import System.Log.Logger

roguestar_version_number :: String
roguestar_version_number = concat $
    intersperse "." $ map show $ versionBranch version

roguestar_program_name :: String
roguestar_program_name = "roguestar-engine"

roguestar_id_string :: String
roguestar_id_string = (roguestar_program_name ++ " " ++ roguestar_version_number)

-- |
-- Processes a single command line argument.
--
runByArgs :: String -> IO ()

runByArgs "tests" =
    do testsPassed <- runAllTests ([sampleTestCase] ++
                                   insidenessTests ++
                                   gridRayCasterTests)
       if testsPassed
           then putStrLn "All tests passed."
           else putStrLn "Error: a test failed."

runByArgs "version" = do putStrLn roguestar_id_string

runByArgs "test-terrain-generator" =
    do seed <- randomIO
       let example_terrain = generateExampleTerrain seed
       putStrLn "Terrain Map of (-20..20),(-10..10)"
       mapM_ putStrLn $ prettyPrintTerrain ((-20,20),(-10,10)) example_terrain
       putStrLn "Terrain Map of (5460..5500),(-1010..-990)"
       mapM_ putStrLn $ prettyPrintTerrain ((5460,5500),(-1010,-990)) example_terrain
       putStrLn "Terrain Map of (5461..5501),(-1009..-989)"
       mapM_ putStrLn $ prettyPrintTerrain ((5461,5501),(-1009,-989)) example_terrain

runByArgs "begin" = mainLoop initial_db

runByArgs "over" = putStrLn "over"

runByArgs "debug" = updateGlobalLogger rootLoggerName (setLevel DEBUG)

runByArgs "help" =
    do putStrLn "Commands:"
       putStrLn "begin                    - begin a protocol session (used by GUI clients and experts)"
       putStrLn "debug                    - set debugging verbosity"
       putStrLn "help                     - print this message"
       putStrLn "over                     - print \"over\" on a line by itself"
       putStrLn "tests                    - run a few tests"
       putStrLn "test-terrain-generator   - display an example terrain map"
       putStrLn "version                  - print the version string"

runByArgs invalidArgument =
    do putStrLn ("Error: unrecognized argument: " ++ invalidArgument)
       fail "Unrecognized argument in runByArgs"

--
-- Each argument corresponds to a particular "runByArgs" command.  Run them all in order.
--
main :: IO ()
main =
    do args <- getArgs
       mapM_ runByArgs args

