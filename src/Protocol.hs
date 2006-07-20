module Protocol
    (mainLoop)
    where

import Data.Char
import Control.Monad.State
import CreatureData
import Creature
import Character
import StatsData
import DB
import System.Exit
import Races

mainLoop :: DB_BaseType -> IO ()
mainLoop db0 = do putStrLn "over"
		  next_command <- getLine
		  db1 <- ioDispatch (words $ map toLower next_command) db0
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

ioDispatch unknown_command db0 = do putStrLn ("protocol-error: unknown command " ++ (unwords unknown_command))
				    return db0

dbDispatch :: [String] -> DB String

dbDispatch ["query","state"] = 
    do state <- dbState
       return $ case state of
			   DBRaceSelectionState -> "race-selection"
			   DBClassSelectionState {} -> "class-selection"

dbDispatch ["action","select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatch ["query","player-stats"] = dbRequiresClassSelectionState dbQueryCreatureStats

dbDispatch ["query","eligable-base-classes"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatch unrecognized = return ("protocol-error: unrecognized request `" ++ (unwords unrecognized) ++ "`")

dbSelectPlayerRace :: String -> DB String
dbSelectPlayerRace race_name = case (selectPlayerRace race_name)
			       of
			       Nothing -> return ("protocol-error: unrecognized race '" ++ race_name ++ "'")
			       Just species -> do dbGenerateInitialPlayerCreature species
						  done

dbQueryCreatureStats :: Creature -> DB String
dbQueryCreatureStats creature = return $ creatureStatsTable creature

creatureStatsTable :: Creature -> String
creatureStatsTable creature =
    let sts = creature_stats creature
	in "begin-table property value\n" ++
	       "str " ++ (show $ str sts) ++ "\n" ++
	       "dex " ++ (show $ dex sts) ++ "\n" ++
	       "con " ++ (show $ con sts) ++ "\n" ++
	       "int " ++ (show $ int sts) ++ "\n" ++
	       "per " ++ (show $ per sts) ++ "\n" ++
	       "cha " ++ (show $ cha sts) ++ "\n" ++
	       "mind " ++ (show $ mind sts) ++ "\n" ++
	       "hp " ++ (show $ hitPoints creature) ++ "\n" ++
	       "maxHP " ++ (show $ maxHitPoints creature) ++ "\n" ++
	       "speed " ++ (show $ creatureSpeed creature) ++ "\n" ++
	       "gender " ++ (show $ creatureGender creature) ++ "\n" ++
	       "end-table"

dbQueryBaseClasses :: Creature -> DB String
dbQueryBaseClasses creature = return $ eligableBaseClassesTable creature

eligableBaseClassesTable :: Creature -> String
eligableBaseClassesTable creature = 
    "begin-table class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"