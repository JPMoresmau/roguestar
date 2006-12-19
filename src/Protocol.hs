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

module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List as List
import Control.Monad.State
import CreatureData
import Creature
import Character
import StatsData
import DB
import DBData
import System.Exit
import Races
import System.IO
import BeginGame
import Data.Maybe
import Plane
import FactionData
import PlaneVisibility
import Facing

mainLoop :: DB_BaseType -> IO ()
mainLoop db0 = do next_command <- getLine
		  db1 <- ioDispatch (words $ map toLower next_command) db0
		  putStrLn "over"
		  hFlush stdout
		  mainLoop db1

done :: DB String
done = return "done"

-- |
-- Perform an action assuming the database is in the DBRaceSelectionState,
-- otherwise returns an error message.
--
dbRequiresRaceSelectionState :: DB String -> DB String
dbRequiresRaceSelectionState action = do state <- dbState
					 case state of
						    DBRaceSelectionState -> action
						    _ -> return $ "protocol-error: not in race selection state (" ++ show state ++ ")"

-- |
-- Perform an action assuming the database is in the DBClassSelectionState,
-- otherwise returns an error message.
--
dbRequiresClassSelectionState :: (Creature -> DB String) -> DB String
dbRequiresClassSelectionState action = do state <- dbState
					  case state of
						     DBClassSelectionState creature -> action creature
						     _ -> return $ "protocol-error: not in class selection state (" ++ show state ++ ")"

-- |
-- Perform an action based on a player creature, when the state is appropriate.
-- The states that work for this are:
--
-- DBClassSelectionState
-- DBPlayerCreatureTurn
--
dbRequiresPlayerCenteredState :: (Creature -> DB String) -> DB String
dbRequiresPlayerCenteredState action =
    do state <- dbState
       case state of
		  DBClassSelectionState creature -> action creature
		  DBPlayerCreatureTurn creature_ref -> action =<< dbGetCreature creature_ref
		  _ -> return $ "protocol-error: not in player-centered state (" ++ show state ++ ")"

-- |
-- Perform an action that works during any creature's turn in a planar environment.
-- The states that work for this are:
--
-- DBPlayerCreatureTurn
-- 
--
dbRequiresPlanarTurnState :: (CreatureRef -> DB String) -> DB String
dbRequiresPlanarTurnState action =
    do state <- dbState
       case state of
		  DBPlayerCreatureTurn creature_ref -> action creature_ref
		  _ -> return $ "protocol-error: not in planar turn state (" ++ show state ++ ")"

-- |
-- Perform an action that works only during a player-character's turn.
-- The states that work for this are:
--
-- DBPlayerCreatureTurn
--
dbRequiresPlayerTurnState :: (CreatureRef -> DB String) -> DB String
dbRequiresPlayerTurnState action =
    do state <- dbState
       case state of
                  DBPlayerCreatureTurn creature_ref -> action creature_ref
                  _ -> return $ "protocol-error: not in player turn state (" ++ show state ++ ")"

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
			   DBPlayerCreatureTurn {} -> "answer: state player-turn"

dbDispatch ["query","player-races"] =
    return ("begin-table player-races 0 name\n" ++
	    unlines player_race_names ++
	    "end-table")

dbDispatch ["query","visible-terrain"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       terrain_map <- maybe (return []) (dbGetVisibleTerrainForFaction Player) maybe_plane_ref 
       return ("begin-table visible-terrain 0 x y terrain-type\n" ++
	       (unlines $ map (\ (terrain_type,(x,y)) -> unwords [show x, show y, show terrain_type]) terrain_map) ++
	       "end-table")

dbDispatch ["query","visible-objects"] = 
    do maybe_plane_ref <- dbGetCurrentPlane
       objects <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
       table_rows <- mapM dbObjectToTableRow objects
       return ("begin-table visible-objects 0 object-unique-id x y facing\n" ++
               (unlines $ table_rows) ++
               "end-table")
        where dbObjectToTableRow obj_ref = 
                do maybe_loc <- dbWhere obj_ref
                   return $ case (toCoordinateFacingLocation . snd =<< maybe_loc :: Maybe ((Integer,Integer),Facing))
                                 of
                                 Just ((x,y),facing) -> unwords [show $ toUID obj_ref,show x,show y,show facing]
                                 Nothing -> ""

dbDispatch ["query","object-details"] =
  do maybe_plane_ref <- dbGetCurrentPlane
     objects <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
     liftM (concat . intersperse "\n") $ mapM dbObjectToTable objects
     where dbObjectToTable obj_ref =
             do table_data <- dbGetObjectTableData obj_ref
                return ("begin-table object-details " ++ (show $ toUID obj_ref) ++ " property value\n" ++
                        table_data ++
                        "end-table")
           dbGetObjectTableData (DBCreatureRef creature_ref) = liftM creatureToTableData $ dbGetCreature creature_ref
           dbGetObjectTableData (DBPlaneRef _) = error "implausible case"
           creatureToTableData creature = "object-type creature\n" ++
                                          (concat $ map (\x -> fst x ++ " " ++ snd x ++ "\n") $ creatureStatsData creature)

dbDispatch ["action","select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatch ["action","reroll"] =
    dbRequiresClassSelectionState $ dbRerollRace

dbDispatch ["action","select-class",class_name] =
    dbRequiresClassSelectionState $ dbSelectPlayerClass class_name

dbDispatch ["action","move",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\x -> dbWalkCreature (fromJust $ stringToFacing direction) x >> done)

dbDispatch ["query","player-stats"] = dbRequiresPlayerCenteredState dbQueryPlayerStats

dbDispatch ["query","center-coordinates"] = dbRequiresPlanarTurnState dbQueryCenterCoordinates

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
	in case find (\x -> (map toLower . show) x == class_name) eligable_base_classes
	   of
	   Nothing -> return ("protocol-error: unrecognized or invalid class '" ++ class_name ++ "'")
	   Just the_class -> do dbBeginGame creature the_class
			        done

dbRerollRace :: Creature -> DB String
dbRerollRace _ = do starting_race <- dbGetStartingRace
		    dbGenerateInitialPlayerCreature $ fromJust starting_race
		    done

dbQueryPlayerStats :: Creature -> DB String
dbQueryPlayerStats creature = return $ playerStatsTable creature

-- |
-- Information about player creatures (for which the player should have almost all available information.)
--
playerStatsTable :: Creature -> String
playerStatsTable c =
    "begin-table player-stats 0 property value\n" ++
               "str " ++ (show $ str c) ++ "\n" ++
	       "dex " ++ (show $ dex c) ++ "\n" ++
	       "con " ++ (show $ con c) ++ "\n" ++
	       "int " ++ (show $ int c) ++ "\n" ++
	       "per " ++ (show $ per c) ++ "\n" ++
	       "cha " ++ (show $ cha c) ++ "\n" ++
	       "mind " ++ (show $ mind c) ++ "\n" ++
	       "hp " ++ (show $ creatureScore HitPoints c) ++ "\n" ++
	       "maxhp " ++ (show $ creatureScore MaxHitPoints c) ++ "\n" ++
	       "species " ++ (creature_species_name c) ++ "\n" ++
	       "random-id " ++ (show $ creature_random_id c) ++ "\n" ++
	       "effective-level " ++ (show $ creatureScore EffectiveLevel c) ++ "\n" ++
	       "gender " ++ (show $ creatureGender c) ++ "\n" ++
	       "end-table"

-- |
-- Information about non-player creatures (for which there are very strict limits on what information
-- the player can have).  The result is in (Property,Value) form so that the result can easily be
-- manipulated by the caller.
--
creatureStatsData :: Creature -> [(String,String)]
creatureStatsData c = [("percent-hp",show $ (creatureScore HitPoints c * 100) `div` creatureScore MaxHitPoints c),
                       ("species",creature_species_name c),
                       ("random-id",show $ creature_random_id c)]

dbQueryBaseClasses :: Creature -> DB String
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> String
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"

dbQueryCenterCoordinates :: CreatureRef -> DB String
dbQueryCenterCoordinates creature_ref =
    do maybe_loc <- dbWhere creature_ref
       case (return . snd) =<< maybe_loc of
		Just (DBCoordinateLocation (x,y)) -> return (begin_table ++
							     "x " ++ show x ++ "\n" ++
							     "y " ++ show y ++ "\n" ++
							     "end-table")
                Just (DBCoordinateFacingLocation ((x,y),facing)) -> return (begin_table ++
                                                                            "x " ++ show x ++ "\n" ++
                                                                            "y " ++ show y ++ "\n" ++
                                                                            "facing " ++ show facing ++ "\n" ++
                                                                            "end-table")
		_ -> return (begin_table ++ "end-table")
	   where begin_table = "begin-table center-coordinates 0 axis coordinate\n"
