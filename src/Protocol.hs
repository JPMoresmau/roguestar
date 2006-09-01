module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List
import Control.Monad.State
import CreatureData
import Creature
import Character
import StatsData
import DB
import System.Exit
import Races
import System.IO
import BeginGame
import Data.Maybe

mainLoop :: DB_BaseType -> IO ()
mainLoop db0 = do next_command <- getLine
		  db1 <- ioDispatch (words $ map toLower next_command) db0
		  putStrLn "over"
		  hFlush stdout
		  mainLoop db1

done :: DB String
done = return "done"

-- |
-- Runs a database action assuming the database is in the DBRaceSelectionState,
-- otherwise returns an error message.
--
dbRequiresRaceSelectionState :: DB String -> DB String
dbRequiresRaceSelectionState action = do state <- dbState
					 case state of
						    DBRaceSelectionState -> action
						    _ -> return "protocol-error: not in race selection state"

-- |
-- Runs a database action assuming the database is in the DBClassSelectionState,
-- otherwise returns an error message.
--
dbRequiresClassSelectionState :: (Creature -> DB String) -> DB String
dbRequiresClassSelectionState action = do state <- dbState
					  case state of
						     DBClassSelectionState creature -> action creature
						     _ -> return "protocol-error: not in class selection state"

ioDispatch :: [String] -> DB_BaseType -> IO DB_BaseType

ioDispatch ["quit"] _ = exitWith ExitSuccess

ioDispatch ["reset"] _ = do db0 <- initialDB
			    putStrLn "done"
			    return db0

ioDispatch ("game":args) db0 = let (outstr,db1) = runState (dbDispatch args) db0
				    in do putStrLn (map toLower outstr)
					  return db1

ioDispatch ("save":_) db0 = do putStrLn "error: save not implemented"
			       return db0

ioDispatch ("load":_) db0 = do putStrLn "error: load not implemented"
			       return db0

ioDispatch ("noop":_) db0 = return db0

ioDispatch unknown_command db0 = do putStrLn ("protocol-error: unknown command " ++ (unwords unknown_command))
				    return db0

dbDispatch :: [String] -> DB String

dbDispatch ["query","state"] = 
    do state <- dbState
       return $ case state of
			   DBRaceSelectionState -> "answer: state race-selection"
			   DBClassSelectionState {} -> "answer: state class-selection"

dbDispatch ["query","player-races"] =
    return ("begin-table player-races 0 name\n" ++
	    unlines player_race_names ++
	    "end-table")

dbDispatch ["action","select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatch ["action","reroll"] =
    dbRequiresClassSelectionState $ dbRerollRace

dbDispatch ["action","select-class",class_name] =
    dbRequiresClassSelectionState $ dbSelectPlayerClass class_name

dbDispatch ["query","player-stats"] = dbRequiresClassSelectionState dbQueryPlayerStats

dbDispatch ["query","base-classes"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatch unrecognized = return ("protocol-error: unrecognized request `" ++ (unwords unrecognized) ++ "`")

dbSelectPlayerRace :: String -> DB String
dbSelectPlayerRace race_name = case (selectPlayerRace race_name)
			       of
			       Nothing -> return ("protocol-error: unrecognized race '" ++ race_name ++ "'")
			       Just species -> do dbGenerateInitialPlayerCreature species
						  done

dbSelectPlayerClass :: String -> Creature -> DB String
dbSelectPlayerClass class_name creature = 
    let eligable_base_classes = getEligableBaseCharacterClasses creature
	in case findIndex (\x -> (map toLower . show) x == class_name) eligable_base_classes
	   of
	   Nothing -> return ("protocol-error: unrecognized or invalid class '" ++ class_name ++ "'")
	   Just i -> do dbBeginGame creature (eligable_base_classes !! i)
			done

dbRerollRace :: Creature -> DB String
dbRerollRace _ = do starting_race <- dbGetStartingRace
		    dbGenerateInitialPlayerCreature $ fromJust starting_race
		    done

dbQueryPlayerStats :: Creature -> DB String
dbQueryPlayerStats creature = return $ playerStatsTable creature

playerStatsTable :: Creature -> String
playerStatsTable creature =
    let sts = creature_stats creature
	in "begin-table player-stats 0 property value\n" ++
	       "str " ++ (show $ str sts) ++ "\n" ++
	       "dex " ++ (show $ dex sts) ++ "\n" ++
	       "con " ++ (show $ con sts) ++ "\n" ++
	       "int " ++ (show $ int sts) ++ "\n" ++
	       "per " ++ (show $ per sts) ++ "\n" ++
	       "cha " ++ (show $ cha sts) ++ "\n" ++
	       "mind " ++ (show $ mind sts) ++ "\n" ++
	       "hp " ++ (show $ hitPoints creature) ++ "\n" ++
	       "maxhp " ++ (show $ maxHitPoints creature) ++ "\n" ++
	       "gender " ++ (show $ creatureGender creature) ++ "\n" ++
	       "end-table"

dbQueryBaseClasses :: Creature -> DB String
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> String
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"