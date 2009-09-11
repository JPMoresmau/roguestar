{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, PatternGuards #-}

module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List as List
import CreatureData
import Creature
import Character
import DB
import DBData
import System.Exit
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
import SpeciesData
import Species
import Data.Ord
import Combat
import Substances
import PlayerState
import Make
-- Don't call dbBehave, use dbPerformPlayerTurn
import Behavior hiding (dbBehave)
-- We need to construct References based on UIDs, so we cheat a little:
import DBPrivate (Reference(ToolRef))

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
-- * ClassSelectionState
-- * PlayerCreatureTurn
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
-- * PlayerCreatureTurn
-- * SnapshotEvent
--
dbRequiresPlanarTurnState :: (DBReadable db) => (CreatureRef -> db String) -> db String
dbRequiresPlanarTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       maybe (return $ "protocol-error: not in planar turn state (" ++ show state ++ ")") action $ creatureOf state

-- |
-- Perform an action that works only during a player-character's turn.
-- The states that work for this are:
--
-- PlayerCreatureTurn
--
dbRequiresPlayerTurnState :: (DBReadable db) => (CreatureRef -> db String) -> db String
dbRequiresPlayerTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
                  PlayerCreatureTurn creature_ref _ -> action creature_ref
                  _ -> return $ "protocol-error: not in player turn state (" ++ show state ++ ")"

-- |
-- For arbitrary-length menu selections, get the current index into the menu, if any.
--
menuState :: (DBReadable db) => db (Maybe Integer)
menuState = liftM menuIndex playerState

-- |
-- For arbitrary-length menu selections, modify the current index into the menu.  If there is no menu index
-- in the current state, this has no effect.
--
modifyMenuState :: (Integer -> Integer) -> DB ()
modifyMenuState f_ = 
    do number_of_tools <- liftM genericLength toolMenuElements
       let f = (\x -> if number_of_tools == 0 then 0 else x `mod` number_of_tools) . f_
       setPlayerState . modifyMenuIndex f =<< playerState

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
                           PlayerCreatureTurn _ MoveMode -> "answer: state move"
                           PlayerCreatureTurn _ (PickupMode {}) -> "answer: state pickup"
                           PlayerCreatureTurn _ (DropMode {}) -> "answer: state drop"
                           PlayerCreatureTurn _ (WieldMode {}) -> "answer: state wield"
                           PlayerCreatureTurn _ AttackMode -> "answer: state attack"
                           PlayerCreatureTurn _ FireMode -> "answer: state fire"
                           PlayerCreatureTurn _ JumpMode -> "answer: state jump"
                           PlayerCreatureTurn _ TurnMode -> "answer: state turn"
                           PlayerCreatureTurn _ (MakeMode _ make_prep) | isFinished make_prep -> "answer: state make-finished"
                           PlayerCreatureTurn _ (MakeMode _ make_prep) | needsKind make_prep -> "answer: state make-what"
                           PlayerCreatureTurn _ (MakeMode {}) -> "answer: state make"
                           PlayerCreatureTurn _ ClearTerrainMode -> "answer: state clear-terrain"
                           SnapshotEvent (AttackEvent {}) -> "answer: state attack-event"
                           SnapshotEvent (MissEvent {}) -> "answer: state miss-event"
                           SnapshotEvent (KilledEvent {}) -> "answer: state killed-event"
                           SnapshotEvent (WeaponOverheatsEvent {}) -> "answer: state weapon-overheats-event"
                           SnapshotEvent (WeaponExplodesEvent {}) -> "answer: state weapon-explodes-event"
                           SnapshotEvent (DisarmEvent {}) -> "answer: state disarm-event"
                           SnapshotEvent (SunderEvent {}) -> "answer: state sunder-event"
                           GameOver -> "answer: state game-over"

dbDispatchQuery ["action-count"] =
    do n <- dbActionCount
       return $ "answer: action-count " ++ show n

dbDispatchQuery ["menu-state"] =
    do m_state <- menuState
       return $ case m_state of
           Nothing -> "answer: menu-state 0"
           Just state -> "answer: menu-state " ++ show state

dbDispatchQuery ["who-attacks"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_source_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
	   SnapshotEvent (MissEvent { miss_event_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
           SnapshotEvent (WeaponOverheatsEvent { weapon_overheats_event_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
           SnapshotEvent (WeaponExplodesEvent { weapon_explodes_event_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
           SnapshotEvent (DisarmEvent { disarm_event_source_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
           SnapshotEvent (SunderEvent { sunder_event_source_creature = attacker_ref }) -> "answer: who-attacks " ++ (show $ toUID attacker_ref)
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
           SnapshotEvent (WeaponOverheatsEvent { weapon_overheats_event_weapon = weapon_ref }) -> "answer: weapon-used " ++ (show $ toUID weapon_ref)
           SnapshotEvent (WeaponExplodesEvent { weapon_explodes_event_weapon = weapon_ref }) -> "answer: weapon-used " ++ (show $ toUID weapon_ref)
           SnapshotEvent (SunderEvent { sunder_event_source_weapon = weapon_ref }) -> "answer: weapon-used " ++ (show $ toUID weapon_ref)
	   _ -> "answer: weapon-used 0"

dbDispatchQuery ["tool-hit"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (DisarmEvent { disarm_event_target_tool = tool_ref }) -> "answer: tool-hit " ++ (show $ toUID tool_ref)
           SnapshotEvent (SunderEvent { sunder_event_target_tool = tool_ref }) -> "answer: tool-hit " ++ (show $ toUID tool_ref)
           _ -> "answer: tool-hit 0"

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

dbDispatchQuery ["who-player"] = return "answer: who-player 2"

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
               "tool-type " ++ toolType tool ++ "\n" ++
               "tool " ++ toolName tool ++ "\n"

dbDispatchQuery ["player-stats","0"] = dbRequiresPlayerCenteredState dbQueryPlayerStats

dbDispatchQuery ["center-coordinates","0"] = dbRequiresPlanarTurnState dbQueryCenterCoordinates

dbDispatchQuery ["base-classes","0"] = dbRequiresClassSelectionState dbQueryBaseClasses

dbDispatchQuery ["pickups","0"] = dbRequiresPlayerTurnState $ \creature_ref -> 
    liftM (showToolMenuTable "pickups" "0") $ toolsToMenuTable =<< dbAvailablePickups creature_ref

dbDispatchQuery ["inventory","0"] = dbRequiresPlayerTurnState $ \creature_ref ->
    liftM (showToolMenuTable "inventory" "0") $ toolsToMenuTable =<< dbGetContents creature_ref

dbDispatchQuery ["menu","0"] =
    liftM (showToolMenuTable "menu" "0") $ toolsToMenuTable =<< toolMenuElements

dbDispatchQuery ["menu",s] | Just window_size <- readNumber s =
    do n <- liftM (fromMaybe 0) menuState
       let half_window = window_size `div` 2
       let windowF (x,_,_) = abs (x - (max half_window n)) <= half_window
       liftM (showToolMenuTable "menu" s . filter windowF) $ toolsToMenuTable =<< toolMenuElements

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

dbDispatchAction [direction] | isJust $ stringToFacing direction =
    do state <- playerState
       case state of
           PlayerCreatureTurn _ player_mode -> case player_mode of
               JumpMode ->   dbDispatchAction ["jump",direction]
               TurnMode ->   dbDispatchAction ["turn",direction]
               AttackMode -> dbDispatchAction ["attack",direction]
               FireMode ->   dbDispatchAction ["fire",direction]
               MoveMode ->   dbDispatchAction ["move",direction]
               ClearTerrainMode ->  dbDispatchAction ["clear-terrain",direction]
               _ ->          dbDispatchAction ["normal",direction]
           _ -> return "protocol-error: not in player turn state"

dbDispatchAction ["normal"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref NormalMode) >> done

dbDispatchAction ["normal",direction] | Just face <- stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> 
        do behavior <- facingBehavior creature_ref face
           dbPerformPlayerTurn behavior creature_ref
           done

dbDispatchAction ["move"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref MoveMode) >> done

dbDispatchAction ["move",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Step $ fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatchAction ["jump"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref JumpMode) >> done

dbDispatchAction ["jump",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Behavior.Jump $ fromJust $ stringToFacing direction) creature_ref >> done)

dbDispatchAction ["turn"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref TurnMode) >> done

dbDispatchAction ["turn",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (TurnInPlace $ fromJust $ stringToFacing direction) creature_ref >> done

dbDispatchAction ["clear-terrain"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref ClearTerrainMode) >> done

dbDispatchAction ["clear-terrain",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (ClearTerrain $ fromJust $ stringToFacing direction) creature_ref >> done

dbDispatchAction ["next"] = modifyMenuState (+1) >> done

dbDispatchAction ["prev"] = modifyMenuState (subtract 1) >> done

dbDispatchAction ["select-menu"] =
    do state <- playerState
       i <- menuState
       tool_table <- toolsToMenuTable =<< toolMenuElements
       let selection = maybe "0" (\(_,tool_ref,_) -> show $ toUID tool_ref) $ find (\(n,_,_) -> Just n == i) tool_table
       case state of
           PlayerCreatureTurn _ player_mode -> case player_mode of
               PickupMode {} -> dbDispatchAction ["pickup",selection]
               DropMode {}   -> dbDispatchAction ["drop",selection]
               WieldMode {}  -> dbDispatchAction ["wield",selection]
               MakeMode {}   -> dbDispatchAction ["make-with",selection]
               _ -> return "protocol-error: not in menu selection state"
           _ -> return "protocol-error: not in player turn state"

dbDispatchAction ["make-begin"] = dbRequiresPlayerTurnState $ \creature_ref ->
    setPlayerState (PlayerCreatureTurn creature_ref (MakeMode 0 prepare_make)) >> done

dbDispatchAction ["make-what",what] | (Just device_kind) <- readDeviceKind what =
    do state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode n make_prep) -> (setPlayerState $ PlayerCreatureTurn c $ MakeMode n (make_prep `makeWith` device_kind)) >> done
           _ -> return "protocol-error: not in make or make-what state"

dbDispatchAction ["make-with",tool_uid] =
    do tool_ref <- readUID ToolRef tool_uid
       tool <- dbGetTool tool_ref
       state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode _ make_prep) -> case (hasChromalite tool, hasMaterial tool, hasGas tool) of
               (Just ch,_,_) | needsChromalite make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (ch,tool_ref)) >> done
               (_,Just m,_) | needsMaterial make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (m,tool_ref)) >> done
               (_,_,Just g) | needsGas make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (g,tool_ref)) >> done
               _ | otherwise -> return "error: tool doesn't have needed substance"
           _ -> return "protocol-error: not in make or make-what state"

dbDispatchAction ["make-end"] =
    do state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode _ make_prep) | isFinished make_prep -> dbPerformPlayerTurn (Make make_prep) c >> done
           PlayerCreatureTurn _ (MakeMode {}) -> return "protocol-error: make isn't complete"
           _ -> return "protocol-error: not in make or make-what state"

dbDispatchAction ["pickup"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do pickups <- dbAvailablePickups creature_ref
       case pickups of
           [tool_ref] -> dbPerformPlayerTurn (Pickup tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-there"
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (PickupMode 0))
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
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (DropMode 0))
       done

dbDispatchAction ["drop",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Drop tool_ref) creature_ref
       done

dbDispatchAction ["wield"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do available <- availableWields creature_ref
       case available of
           [tool_ref] -> dbPerformPlayerTurn (Wield tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag "nothing-in-inventory"
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (WieldMode 0))
       done

dbDispatchAction ["wield",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Wield tool_ref) creature_ref
       done

dbDispatchAction ["unwield"] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn Unwield creature_ref >> done 

dbDispatchAction ["fire"] =
    dbRequiresPlayerTurnState $ \creature_ref -> rangedAttackModel creature_ref >> setPlayerState (PlayerCreatureTurn creature_ref FireMode) >> done

dbDispatchAction ["fire",direction] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (Fire $ fromJust $ stringToFacing direction) creature_ref >> done

dbDispatchAction ["attack"] =
    dbRequiresPlayerTurnState $ \creature_ref -> meleeAttackModel creature_ref >> setPlayerState (PlayerCreatureTurn creature_ref AttackMode) >> done

dbDispatchAction ["attack",direction] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (Attack $ fromJust $ stringToFacing direction) creature_ref >> done

dbDispatchAction ["activate"] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn Activate creature_ref >> done

dbDispatchAction unrecognized = return ("protocol-error: unrecognized action `" ++ (unwords unrecognized) ++ "`")

dbSelectPlayerRace :: String -> DB String
dbSelectPlayerRace race_name = case find (\s -> map toLower (show s) == race_name) player_species of
			       Nothing -> return ("protocol-error: unrecognized race '" ++ race_name ++ "'")
			       Just species -> do generateInitialPlayerCreature species
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
		    generateInitialPlayerCreature $ fromJust starting_race
		    done

dbQueryPlayerStats :: (DBReadable db) => Creature -> db String
dbQueryPlayerStats creature = return $ playerStatsTable creature

-- |
-- Generate a list of tools, e.g. for an inventory list or pickup list.
-- The data source is selected on a context-sensitive basis.
--
toolMenuElements :: (DBReadable db) => db [ToolRef]
toolMenuElements =
    do state <- playerState
       case state of
           PlayerCreatureTurn c (PickupMode {}) -> dbAvailablePickups c
           PlayerCreatureTurn c (WieldMode {}) -> availableWields c
           PlayerCreatureTurn c (MakeMode _ make_prep) | needsChromalite make_prep -> filterM (liftM (isJust . hasChromalite) . dbGetTool) =<< availableWields c
           PlayerCreatureTurn c (MakeMode _ make_prep) | needsMaterial make_prep -> filterM (liftM (isJust . hasMaterial) . dbGetTool) =<< availableWields c
           PlayerCreatureTurn c (MakeMode _ make_prep) | needsGas make_prep -> filterM (liftM (isJust . hasGas) . dbGetTool) =<< availableWields c
           PlayerCreatureTurn c _ -> dbGetContents c
           _ -> return []

-- |
-- Convert a list of tool menu elements into table row entries.
-- The result entries consist of an index incrementing from zero, ToolRef, and name of the tool.
--
toolsToMenuTable :: (DBReadable db) => [ToolRef] -> db [(Integer,ToolRef,String)]
toolsToMenuTable raw_uids =
    do let uids = sortBy (comparing toUID) raw_uids
       tool_names <- mapM (liftM toolName . dbGetTool) uids
       return $ zip3 [0..] uids tool_names

-- |
-- Generate a tool menu table in text form, with the specified name and element list.
--
showToolMenuTable :: String -> String -> [(Integer,ToolRef,String)] -> String
showToolMenuTable table_name table_id tool_table = 
    "begin-table " ++ table_name ++ " " ++ table_id ++ " n uid name" ++ "\n" ++
    unlines (map (\(n,uid,tool_name) -> unwords [show n,show $ toUID uid,tool_name]) tool_table) ++
    "end-table"

-- |
-- Information about player creatures (for which the player should have almost all available information.)
--
playerStatsTable :: Creature -> String
playerStatsTable c =
    "begin-table player-stats 0 property value\n" ++
               "str " ++ (show $ rawScore Strength c) ++ "\n" ++
	       "spd " ++ (show $ rawScore Speed c) ++ "\n" ++
	       "con " ++ (show $ rawScore Constitution c) ++ "\n" ++
	       "int " ++ (show $ rawScore Intellect c) ++ "\n" ++
	       "per " ++ (show $ rawScore Perception c) ++ "\n" ++
	       "cha " ++ (show $ rawScore Charisma c) ++ "\n" ++
	       "mind " ++ (show $ rawScore Mindfulness c) ++ "\n" ++
	       "maxhp " ++ (show $ creatureAbilityScore ToughnessTrait c) ++ "\n" ++
	       "species " ++ (show $ creature_species c) ++ "\n" ++
	       "random-id " ++ (show $ creature_random_id c) ++ "\n" ++
	       "gender " ++ (show $ creatureGender c) ++ "\n" ++
	       "end-table"

-- |
-- Information about non-player creatures (for which there are very strict limits on what information
-- the player can have).  The result is in (Property,Value) form so that the result can easily be
-- manipulated by the caller.
--
creatureStatsData :: Creature -> [(String,String)]
creatureStatsData c = [("species",show $ creature_species c),
                       ("random-id",show $ creature_random_id c)]

toolName :: Tool -> String
toolName (DeviceTool _ d) = deviceName d
toolName (Sphere s) = prettySubstance s

toolType :: Tool -> String
toolType (DeviceTool Gun _) = "gun"
toolType (DeviceTool Sword _) = "sword"
toolType (Sphere (GasSubstance _)) = "sphere-gas"
toolType (Sphere (MaterialSubstance _)) = "sphere-material"
toolType (Sphere (ChromaliteSubstance _)) = "sphere-chromalite"

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
    do let m_uid = readNumber x
       ok <- maybe (return False) (dbVerify . f) m_uid
       when (not ok) $ throwError $ DBError $ "protocol-error: " ++ x ++ " is not a valid uid."
       return $ f $ fromJust m_uid

readNumber :: String -> Maybe Integer
readNumber = fmap fst . listToMaybe . filter (null . snd) . readDec

readDeviceKind :: String -> Maybe DeviceKind
readDeviceKind "pistol" = Just Pistol
readDeviceKind "carbine" = Just Carbine
readDeviceKind "rifle" = Just Rifle
readDeviceKind "fleuret" = Just Fleuret
readDeviceKind "sabre" = Just Sabre
readDeviceKind _ = Nothing
