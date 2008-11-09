{-# LANGUAGE ExistentialQuantification, PatternSignatures #-}

module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List as List
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
import PlaneData
import Tool
import FactionData
import PlaneVisibility
import Facing
import ToolData
import Control.Monad.Error
import Numeric
import Turns
-- Don't call dbBehave, use dbPerformPlayerTurn
import Behavior hiding (dbBehave)
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

dbOldestSnapshotOnly :: (DBReadable db) => db ()
dbOldestSnapshotOnly = 
    do b <- dbHasSnapshot
       when b $ fail "protocol-error: pending snapshot"

-- |
-- Perform an action assuming the database is in the DBRaceSelectionState,
-- otherwise returns an error message.
--
dbRequiresRaceSelectionState :: (DBReadable db) => db String -> db String
dbRequiresRaceSelectionState action = 
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
           RaceSelectionState -> action
           _ -> return $ "protocol-error: not in race selection state (" ++ show state ++ ")"

-- |
-- Perform an action assuming the database is in the DBClassSelectionState,
-- otherwise returns an error message.
--
dbRequiresClassSelectionState :: (DBReadable db) => (Creature -> db String) -> db String
dbRequiresClassSelectionState action = 
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
           ClassSelectionState creature -> action creature
           _ -> return $ "protocol-error: not in class selection state (" ++ show state ++ ")"

-- |
-- Perform an action that operates on the player creature (not in any context).
-- The states that work for this are:
--
-- DBClassSelectionState
-- DBPlayerCreatureTurn
--
dbRequiresPlayerCenteredState :: (DBReadable db) => (Creature -> db String) -> db String
dbRequiresPlayerCenteredState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
		  ClassSelectionState creature -> action creature
		  PlayerCreatureTurn creature_ref _ -> action =<< dbGetCreature creature_ref
		  _ -> return $ "protocol-error: not in player-centered state (" ++ show state ++ ")"

-- |
-- Perform an action that works during any creature's turn in a planar environment.
-- The states that work for this are:
--
-- DBPlayerCreaturePickupMode
-- DBEvent
--
dbRequiresPlanarTurnState :: (DBReadable db) => (CreatureRef -> db String) -> db String
dbRequiresPlanarTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
		  PlayerCreatureTurn creature_ref _ -> action creature_ref
		  SnapshotEvent (AttackEvent { attack_event_source_creature = attacker_ref }) -> action attacker_ref
		  SnapshotEvent (MissEvent { miss_event_creature = attacker_ref }) -> action attacker_ref
		  SnapshotEvent (KilledEvent killed_ref) -> action killed_ref
		  _ -> return $ "protocol-error: not in planar turn state (" ++ show state ++ ")"

-- |
-- Perform an action that works only during a player-character's turn.
-- The states that work for this are:
--
-- DBPlayerCreatureTurn
--
dbRequiresPlayerTurnState :: (DBReadable db) => (CreatureRef -> db String) -> db String
dbRequiresPlayerTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
                  PlayerCreatureTurn creature_ref _ -> action creature_ref
                  _ -> return $ "protocol-error: not in player turn state (" ++ show state ++ ")"

ioDispatch :: [String] -> DB_BaseType -> IO DB_BaseType

ioDispatch ["quit"] _ = exitWith ExitSuccess

ioDispatch ["reset"] _ = do putStrLn "done"
			    return initial_db

ioDispatch ("game":game) db0 = 
    do a <- case game of
                ["query","snapshot"] -> runDB (ro $ liftM (\b -> "answer: snapshot " ++ if b then "yes" else "no") $ dbHasSnapshot) db0
                ("query":args) -> runDB (ro $ dbPeepOldestSnapshot $ dbDispatchQuery args) db0
		("action":args) -> runDB (dbDispatchAction args) db0
                _ -> return $ Left $ DBError $ "protocol-error: unrecognized request: `" ++ unwords game ++ "`"
       case a of
           Right (outstr,db1) -> 
	       do putStrLn (map toLower outstr)
	          return db1
           Left (DBErrorFlag errstr) -> 
	       do putStrLn "done"
	          return $ db0 { db_error_flag = errstr }
  	   Left (DBError errstr) -> 
	       do putStrLn (map toLower errstr ++ "\n")
	          return db0

ioDispatch ("save":_) db0 = do putStrLn "engine-error: save not implemented"
			       return db0

ioDispatch ("load":_) db0 = do putStrLn "engine-error: load not implemented"
			       return db0

ioDispatch ("noop":_) db0 = return db0

ioDispatch unknown_command db0 = do putStrLn ("protocol-error: unknown command " ++ (unwords unknown_command))
				    return db0

dbDispatchQuery :: (DBReadable db) => [String] -> db String
dbDispatchQuery ["state"] = 
    do state <- playerState
       return $ case state of
			   RaceSelectionState -> "answer: state race-selection"
			   ClassSelectionState {} -> "answer: state class-selection"
			   PlayerCreatureTurn _ NormalMode -> "answer: state player-turn"
                           PlayerCreatureTurn _ PickupMode -> "answer: state pickup"
                           PlayerCreatureTurn _ DropMode -> "answer: state drop"
                           PlayerCreatureTurn _ WieldMode -> "answer: state wield"
                           SnapshotEvent (AttackEvent {}) -> "answer: state attack"
                           SnapshotEvent (MissEvent {}) -> "answer: state miss"
                           SnapshotEvent (KilledEvent {}) -> "answer: state killed"
                           GameOver -> "answer: state game-over"

dbDispatchQuery ["who-attacks"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_source_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
	   SnapshotEvent (MissEvent { miss_event_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
	   _ -> "answer: who-attacks 0"

dbDispatchQuery ["who-hit"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_target_creature = target_ref }) -> "answer: who-hit " ++ (show $ toUID target_ref)
	   _ -> "answer: who-hit 0"

dbDispatchQuery ["weapon-used"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_source_weapon = Just weapon_ref }) -> "answer: weapon-used " ++ (show $ toUID weapon_ref)
	   SnapshotEvent (MissEvent { miss_event_weapon = Just weapon_ref }) -> "answer: weapon-used " ++ (show $ toUID weapon_ref)
	   _ -> "answer: weapon-used 0"

dbDispatchQuery ["who-killed"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (KilledEvent killed_ref) -> "answer: who-killed " ++ (show $ toUID killed_ref)
	   _ -> "answer: who-killed 0"

dbDispatchQuery ["player-races","0"] =
    return ("begin-table player-races 0 name\n" ++
	    unlines player_race_names ++
	    "end-table")

dbDispatchQuery ["visible-terrain","0"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       terrain_map <- maybe (return []) (dbGetVisibleTerrainForFaction Player) maybe_plane_ref 
       return ("begin-table visible-terrain 0 x y terrain-type\n" ++
	       (unlines $ map (\(terrain_type,Position (x,y)) -> unwords [show x, show y, show terrain_type]) terrain_map) ++
	       "end-table")

dbDispatchQuery ["visible-objects","0"] = 
    do maybe_plane_ref <- dbGetCurrentPlane
       (objects :: [Location S (Reference ()) ()]) <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
       table_rows <- mapM (dbObjectToTableRow . entity) objects
       return ("begin-table visible-objects 0 object-unique-id x y facing\n" ++
               (unlines $ table_rows) ++
               "end-table")
        where dbObjectToTableRow obj_ref = 
                do l <- dbWhere obj_ref
                   return $ case (extractLocation l,extractLocation l) of
                                 (Just (Position (x,y)),maybe_face) -> unwords [show $ toUID obj_ref,show x,show y,show $ fromMaybe Here maybe_face]
                                 _ -> ""

dbDispatchQuery ["object-details",_] = ro $
  do maybe_plane_ref <- dbGetCurrentPlane
     (visibles :: [Reference ()]) <- maybe (return []) (dbGetVisibleObjectsForFaction Player) maybe_plane_ref
     let creature_refs = mapMaybe (coerceReferenceTyped _creature) visibles
     wielded <- liftM catMaybes $ mapM dbGetWielded creature_refs
     let tool_refs = mapMaybe (coerceReferenceTyped _tool) visibles ++ wielded
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

dbDispatchQuery ["player-stats","0"] = dbRequiresPlayerCenteredState dbQueryPlayerStats

dbDispatchQuery ["center-coordinates","0"] = dbRequiresPlanarTurnState dbQueryCenterCoordinates

dbDispatchQuery ["base-classes","0"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatchQuery ["pickups","0"] = dbRequiresPlayerTurnState $ \creature_ref -> 
    do pickups <- dbAvailablePickups creature_ref
       return $ "begin-table pickups 0 uid\n" ++
                unlines (map (show . toUID) pickups) ++
		"end-table"

dbDispatchQuery ["inventory","0"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do (inventory :: [ToolRef]) <- dbGetContents creature_ref
       return $ "begin-table inventory 0 uid\n" ++
                unlines (map (show . toUID) inventory) ++
		"end-table"

dbDispatchQuery ["wielded-objects","0"] =
    do m_plane_ref <- dbGetCurrentPlane
       creature_refs <- maybe (return []) (dbGetVisibleObjectsForFaction Player) m_plane_ref
       wielded_tool_refs <- mapM dbGetWielded creature_refs
       let wieldedPairToTable :: CreatureRef -> Maybe ToolRef -> Maybe String
           wieldedPairToTable creature_ref = fmap (\tool_ref -> (show $ toUID tool_ref) ++ " " ++ (show $ toUID creature_ref))
       return $ "begin-table wielded-objects 0 uid creature\n" ++
                unlines (catMaybes $ zipWith wieldedPairToTable creature_refs wielded_tool_refs) ++
		"end-table"

dbDispatchQuery ["biome"] =
    do m_plane_ref <- dbGetCurrentPlane
       biome_name <- case m_plane_ref of
           Nothing -> return "nothing"
	   Just plane_ref -> liftM (show . plane_biome) $ dbGetPlane plane_ref
       return $ "answer: biome " ++ biome_name

dbDispatchQuery unrecognized = return $ "protocol-error: unrecognized query `" ++ unwords unrecognized ++ "`"

dbDispatchAction :: [String] -> DB String
dbDispatchAction ["continue"] = dbPopOldestSnapshot >> done

dbDispatchAction ["select-race",race_name] = 
    dbRequiresRaceSelectionState $ dbSelectPlayerRace race_name

dbDispatchAction ["reroll"] =
    dbRequiresClassSelectionState $ dbRerollRace

dbDispatchAction ["select-class",class_name] =
    dbRequiresClassSelectionState $ dbSelectPlayerClass class_name

dbDispatchAction ["move",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Step $ fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatchAction ["jump",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Behavior.Jump $ fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatchAction ["turn",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (TurnInPlace $ fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatchAction ["pickup"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do pickups <- dbAvailablePickups creature_ref
       case pickups of
           [tool_ref] -> dbPerformPlayerTurn (Pickup tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-there"
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref PickupMode)
       done

dbDispatchAction ["pickup",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Pickup tool_ref) creature_ref
       done

dbDispatchAction ["drop"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- dbGetContents creature_ref
       case inventory of
           [tool_ref] -> dbPerformPlayerTurn (Drop tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-in-inventory"
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref DropMode)
       done

dbDispatchAction ["drop",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Drop tool_ref) creature_ref
       done

dbDispatchAction ["wield"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- dbGetContents creature_ref
       case inventory of
           [tool_ref] -> dbPerformPlayerTurn (Wield tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-in-inventory"
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref WieldMode)
       done

dbDispatchAction ["wield",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Wield tool_ref) creature_ref
       done

dbDispatchAction ["unwield"] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn Unwield creature_ref >> done 

dbDispatchAction ["fire",direction] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (Fire $ fromJust $ stringToFacing direction) creature_ref >> done

dbDispatchAction unrecognized = return ("protocol-error: unrecognized action `" ++ (unwords unrecognized) ++ "`")

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

dbQueryPlayerStats :: (DBReadable db) => Creature -> db String
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
toolData g@(GunTool {}) = [("tool-type","gun"),
                       ("tool",toolName g)]

dbQueryBaseClasses :: (DBReadable db) => Creature -> db String
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> String
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" ++
    (unlines $ map show $ getEligableBaseCharacterClasses creature) ++
    "end-table"

dbQueryCenterCoordinates :: (DBReadable db) => CreatureRef -> db String
dbQueryCenterCoordinates creature_ref =
    do l <- dbWhere creature_ref
       case (extractLocation l,extractLocation l :: Maybe Facing) of
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
       
