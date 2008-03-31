
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
import Tool
import FactionData
import PlaneVisibility
import Facing
import ToolData
import Control.Monad.Error
import Numeric
-- We need to construct References based on UIDs, so we cheat a little:
import DBPrivate (Reference(..))

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
		  DBPlayerCreatureTurn creature_ref _ -> action =<< dbGetCreature creature_ref
		  _ -> return $ "protocol-error: not in player-centered state (" ++ show state ++ ")"

-- |
-- Perform an action that works during any creature's turn in a planar environment.
-- The states that work for this are:
--
-- DBPlayerCreaturePickupMode 
--
dbRequiresPlanarTurnState :: (CreatureRef -> DB String) -> DB String
dbRequiresPlanarTurnState action =
    do state <- dbState
       case state of
		  DBPlayerCreatureTurn creature_ref _ -> action creature_ref
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
                  DBPlayerCreatureTurn creature_ref _ -> action creature_ref
                  _ -> return $ "protocol-error: not in player turn state (" ++ show state ++ ")"

ioDispatch :: [String] -> DB_BaseType -> IO DB_BaseType

ioDispatch ["quit"] _ = exitWith ExitSuccess

ioDispatch ["reset"] _ = do db0 <- initialDB
			    putStrLn "done"
			    return db0

ioDispatch ("game":args) db0 = 
    case runState (runErrorT $ dbDispatch args) db0 of
        (Right outstr,db1) -> do putStrLn (map toLower outstr)
				 return db1
        (Left (DBErrorFlag errstr),_) -> do putStrLn "done"
	                                    return $ db0 { db_error_flag = errstr }
	(Left (DBError errstr),_) -> do putStrLn ("error: " ++ map toLower errstr ++ "\n")
	                                return db0

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
			   DBPlayerCreatureTurn _ DBNotSpecial -> "answer: state player-turn"
                           DBPlayerCreatureTurn _ DBPickupMode -> "answer: state pickup"
                           DBPlayerCreatureTurn _ DBDropMode -> "answer: state drop"
                           DBPlayerCreatureTurn _ DBWieldMode -> "answer: state wield"

dbDispatch ["query","player-races","0"] =
    return ("begin-table player-races 0 name\n" ++
	    unlines player_race_names ++
	    "end-table")

dbDispatch ["query","visible-terrain","0"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       terrain_map <- maybe (return []) (dbGetVisibleTerrainForFaction Player) maybe_plane_ref 
       return ("begin-table visible-terrain 0 x y terrain-type\n" ++
	       (unlines $ map (\(terrain_type,Position (x,y)) -> unwords [show x, show y, show terrain_type]) terrain_map) ++
	       "end-table")

dbDispatch ["query","visible-objects","0"] = 
    do maybe_plane_ref <- dbGetCurrentPlane
       objects <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
       table_rows <- mapM dbObjectToTableRow objects
       return ("begin-table visible-objects 0 object-unique-id x y facing\n" ++
               (unlines $ table_rows) ++
               "end-table")
        where dbObjectToTableRow obj_ref = 
                do loc <- dbWhere obj_ref
                   return $ case (position loc,facing loc) of
                                 (Just (Position (x,y)),maybe_face) -> unwords [show $ toUID obj_ref,show x,show y,maybe "Here" show maybe_face]
                                 _ -> ""

dbDispatch ["query","object-details",_] = ro $
  do maybe_plane_ref <- dbGetCurrentPlane
     visibles <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
     let creature_refs = mapMaybe toCreatureRef visibles
     wielded <- liftM catMaybes $ mapM dbGetWielded creature_refs
     let tool_refs = mapMaybe toToolRef visibles ++ wielded
     creatures <- liftM (zip creature_refs) $ mapRO dbGetCreature creature_refs
     tools <- liftM (zip tool_refs)$ mapRO dbGetTool tool_refs
     return $ unlines $ (map creatureToTableData creatures ++
                         map toolToTableData tools)
     where objectTableWrapper obj_ref table_data =
               ("begin-table object-details " ++
                   (show $ toUID obj_ref) ++
                   " property value\n" ++
                   table_data ++
                   "end-table")
           creatureToTableData :: (CreatureRef,Creature) -> String
           creatureToTableData (ref,creature) = objectTableWrapper ref $
               "object-type creature\n" ++
               (concat $ map (\x -> fst x ++ " " ++ snd x ++ "\n") $ creatureStatsData creature)
           toolToTableData :: (ToolRef,Tool) -> String
           toolToTableData (ref,tool) = objectTableWrapper ref $
               "object-type tool\n" ++
               (concat $ map (\x -> fst x ++ " " ++ snd x ++ "\n") $ toolData tool)

dbDispatch ["action","select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatch ["action","reroll"] =
    dbRequiresClassSelectionState $ dbRerollRace

dbDispatch ["action","select-class",class_name] =
    dbRequiresClassSelectionState $ dbSelectPlayerClass class_name

dbDispatch ["action","move",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbStepCreature (fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatch ["action","turn",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbTurnCreature (fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatch ["action","pickup"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do pickups <- dbAvailablePickups creature_ref
       case pickups of
           [tool_ref] -> dbMove (dbPickupTool creature_ref) tool_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-there"
	   _ -> dbSetState (DBPlayerCreatureTurn creature_ref DBPickupMode)
       done

dbDispatch ["action","pickup",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbMove (dbPickupTool creature_ref) tool_ref
       done

dbDispatch ["action","drop"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- dbGetCarried creature_ref
       case inventory of
           [tool_ref] -> dbMove dbDropTool tool_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-in-inventory"
	   _ -> dbSetState (DBPlayerCreatureTurn creature_ref DBDropMode)
       done

dbDispatch ["action","drop",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       tool_parent <- liftM extractLocation $ dbWhere tool_ref
       when (tool_parent /= Just creature_ref) $ throwError $ DBErrorFlag "not-in-inventory"
       dbMove dbDropTool tool_ref
       done

dbDispatch ["action","wield"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- dbGetInventory creature_ref
       case inventory of
           [tool_ref] -> dbMove dbWieldTool tool_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-in-inventory"
	   _ -> dbSetState (DBPlayerCreatureTurn creature_ref DBWieldMode)
       done

dbDispatch ["action","wield",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       tool_parent <- liftM extractLocation $ dbWhere tool_ref
       when (tool_parent /= Just creature_ref) $ throwError $ DBErrorFlag "not-in-inventory"
       dbMove dbWieldTool tool_ref
       done

dbDispatch ["action","unwield"] = dbRequiresPlayerTurnState $ \c -> dbUnwieldCreature c >> done 

dbDispatch ["query","player-stats","0"] = dbRequiresPlayerCenteredState dbQueryPlayerStats

dbDispatch ["query","center-coordinates","0"] = dbRequiresPlanarTurnState dbQueryCenterCoordinates

dbDispatch ["query","base-classes","0"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatch ["query","pickups","0"] = dbRequiresPlayerTurnState $ \creature_ref -> 
    do pickups <- dbAvailablePickups creature_ref
       return $ "begin-table pickups 0 uid\n" ++
                unlines (map (show . toUID) pickups) ++
		"end-table"

dbDispatch ["query","inventory","0"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- liftM (map entity . mapMaybe toToolLocation) $ dbGetContents creature_ref
       return $ "begin-table inventory 0 uid\n" ++
                unlines (map (show . toUID) inventory) ++
		"end-table"

dbDispatch ["query","wielded-objects","0"] =
    do m_plane_ref <- dbGetCurrentPlane
       creature_refs <- liftM (mapMaybe toCreatureRef) $ maybe (return []) (dbGetVisibleObjectsForFaction Player) m_plane_ref
       wielded_tool_refs <- mapM dbGetWielded creature_refs
       let wieldedPairToTable :: CreatureRef -> Maybe ToolRef -> Maybe String
           wieldedPairToTable creature_ref = fmap (\tool_ref -> (show $ toUID tool_ref) ++ " " ++ (show $ toUID creature_ref))
       return $ "begin-table wielded-objects 0 uid creature\n" ++
                unlines (catMaybes $ zipWith wieldedPairToTable creature_refs wielded_tool_refs) ++
		"end-table"

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
	in case find (\x -> (map toLower . show) x == class_name) eligable_base_classes of
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

-- |
-- Information about non-owned tools.
--
toolData :: Tool -> [(String,String)]
toolData g@(Gun {}) = [("tool-type","gun"),
                       ("tool",toolName g)]

dbQueryBaseClasses :: Creature -> DB String
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> String
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"

dbQueryCenterCoordinates :: CreatureRef -> DB String
dbQueryCenterCoordinates creature_ref =
    do loc <- dbWhere creature_ref
       case (position loc,facing loc) of
		(Just (Position (x,y)),Nothing) -> 
                    return (begin_table ++
			    "x " ++ show x ++ "\n" ++
			    "y " ++ show y ++ "\n" ++
			    "end-table")
                (Just (Position (x,y)),Just face) -> 
                    return (begin_table ++
                           "x " ++ show x ++ "\n" ++
                           "y " ++ show y ++ "\n" ++
                           "facing " ++ show face ++ "\n" ++
                           "end-table")
		_ -> return (begin_table ++ "end-table")
	   where begin_table = "begin-table center-coordinates 0 axis coordinate\n"

readUID :: (Integer -> Reference a) -> String -> DB (Reference a)
readUID f x = 
    do let m_uid = fmap fst $ listToMaybe $ filter (null . snd) $ readDec x
       ok <- maybe (return False) (dbVerify . f) m_uid
       when (not ok) $ throwError $ DBError $ "protocol-error: " ++ x ++ " is not a valid uid."
       return $ f $ fromJust m_uid
       
