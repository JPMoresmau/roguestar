{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, PatternGuards, OverloadedStrings #-}

module Protocol
    (mainLoop)
    where

import Data.Char
import Data.List as List
import CreatureData
import Creature
import Character
import DB
import System.Exit
import System.IO
import BeginGame
import Data.Maybe
import Plane
import PlaneData
import Building
import BuildingData
import Tool
import FactionData
import PlaneVisibility
import Facing
import ToolData
import Control.Monad.Error
import Turns
import SpeciesData
import Species
import Data.Ord
import Combat
import Substances
import PlayerState
import Make
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import WorkCluster
import qualified Data.ByteString.Char8 as B
import qualified Perception
-- Don't call dbBehave, use dbPerformPlayerTurn
import Behavior hiding (dbBehave)
-- We need to construct References based on UIDs, so we cheat a little:
import DBPrivate (Reference(ToolRef))

mainLoop :: DB_BaseType -> IO ()
mainLoop db_init = 
    do db_var <- newMVar db_init
       input_chan <- newChan
       output_chan <- newChan
       query_count <- newTVarIO (Just 0) -- Just (the number of running queries) or Nothing (a non-query action is in progress)
       wait_quit <- newEmptyMVar
       work_cluster <- newWorkCluster
       replaceWorkOperation work_cluster . evaluateGame =<< readMVar db_var
       let foreverLoopThenQuit = flip finally (putMVar wait_quit ()) . forever
       _ <- forkIO $ foreverLoopThenQuit $ writeChan input_chan =<< B.getLine
       _ <- forkIO $ foreverLoopThenQuit $
           do next_line <- liftM (B.map toLower . B.unlines . B.lines) (readChan output_chan)
              when (B.length next_line > 0) $
                  do B.putStrLn next_line
                     B.putStrLn "over"
              hFlush stdout
       _ <- forkIO $ foreverLoopThenQuit $
           do next_command <- readChan input_chan
              case (B.words $ B.map toLower next_command) of
                  ["quit"] -> exitWith ExitSuccess
                  ["reset"] -> stopping query_count $ modifyMVar_ db_var (const $ return initial_db)
                  ("game":"query":args) -> 
                      do querrying query_count $
                             do result <- workRequest work_cluster (Query, args)
                                complete Nothing output_chan result
                  ("game":"action":args) ->
                      do result <- workRequest work_cluster (Action, args)
                         stopping query_count $ complete (Just db_var) output_chan result
                         querrying query_count $ complete Nothing output_chan result -- print the result as a query, this will ensure errors get printed
                         replaceWorkOperation work_cluster . evaluateGame =<< readMVar db_var
                  ("noop":_) -> return ()
                  failed -> 
                      do _ <- forkIO $ complete Nothing output_chan $ Left $ DBError $ "protocol-error: unrecognized request: `" ++ B.unpack (B.unwords failed) ++ "`"
                         return ()
       takeMVar wait_quit -- "park" the main function

-- | Evaluate a 'GameDirective' and return it from a remote thread via an 'MVar'.
evaluateGame :: DB_BaseType -> WorkRequest -> IO WorkResult
evaluateGame db0 (Query, ["snapshot"]) = (runDB $ ro $ liftM (\b -> "answer: snapshot " `B.append` if b then "yes" else "no") dbHasSnapshot) db0
evaluateGame db0 (Query, args) = (runDB $ ro $ dbPeepOldestSnapshot $ dbDispatchQuery args) db0
evaluateGame db0 (Action, args) = runDB (liftM (const "") $ dbDispatchAction args) db0

-- | Wait for currently running queries to finish, and stop processing incomming queries while we mutate the database.
stopping :: TVar (Maybe Integer) -> IO () -> IO ()
stopping query_count actionM = bracket
    (atomically $ do maybe retry (\x -> when (x /= 0) retry) =<< readTVar query_count
                     writeTVar query_count $ Nothing)
    (const $ atomically $ writeTVar query_count (Just 0))
    (const actionM)

-- | Process a querry concurrently with other queries.
querrying :: TVar (Maybe Integer) -> IO () -> IO ()
querrying query_count actionM =
    do atomically $ writeTVar query_count =<< liftM Just . (maybe retry $ return . (+1)) =<< readTVar query_count
       _ <- forkIO $ finally (atomically $ do writeTVar query_count =<< liftM (fmap (subtract 1)) (readTVar query_count)) actionM
       return ()

-- | Complete a querry or action.  If a database variable is provided, it will be modified according to the result of the action.
-- The result of the action will be printed to the output_chan.
complete :: Maybe (MVar DB_BaseType) -> Chan B.ByteString -> Either DBError (B.ByteString,DB_BaseType) -> IO ()
complete m_db_var output_chan result =
    do case m_db_var of
           Just db_var -> 
               do modifyMVar_ db_var $ \db0 -> return $ case result of
                      Right (_,db1) -> db1
                      Left (DBErrorFlag errflag) -> db0 { db_error_flag = show errflag }
                      Left (DBError _) -> db0
                  writeChan output_chan "done"
           Nothing ->
               do case result of
                      Right (outstr,_) ->
                          do _ <- evaluate outstr
                             writeChan output_chan outstr
                      Left (DBErrorFlag _) -> return () -- client will query this explicitly (if it cares)
                      Left (DBError errstr) ->
                          do writeChan output_chan $ "error: " `B.append` B.pack errstr
                             B.hPutStrLn stderr $ "DBError: " `B.append` B.pack errstr

dbOldestSnapshotOnly :: (DBReadable db) => db ()
dbOldestSnapshotOnly = 
    do b <- dbHasSnapshot
       when b $ fail "protocol-error: pending snapshot"

-- |
-- Perform an action assuming the database is in the DBRaceSelectionState,
-- otherwise returns an error message.
--
dbRequiresRaceSelectionState :: (DBReadable db) => db a -> db a
dbRequiresRaceSelectionState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
           RaceSelectionState -> action
           _ -> throwError $ DBError $ "protocol-error: not in race selection state (" ++ show state ++ ")"

-- |
-- Perform an action assuming the database is in the DBClassSelectionState,
-- otherwise returns an error message.
--
dbRequiresClassSelectionState :: (DBReadable db) => (Creature -> db a) -> db a
dbRequiresClassSelectionState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
           ClassSelectionState creature -> action creature
           _ -> throwError $ DBError $ "protocol-error: not in class selection state (" ++ show state ++ ")"

-- |
-- Perform an action that operates on the player creature (not in any context).
-- The states that work for this are:
--
-- * ClassSelectionState
-- * PlayerCreatureTurn
--
dbRequiresPlayerCenteredState :: (DBReadable db) => (Creature -> db a) -> db a
dbRequiresPlayerCenteredState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
                  ClassSelectionState creature -> action creature
                  PlayerCreatureTurn creature_ref _ -> action =<< dbGetCreature creature_ref
                  _ -> throwError $ DBError $ "protocol-error: not in player-centered state (" ++ show state ++ ")"

-- |
-- Perform an action that works during any creature's turn in a planar environment.
-- The states that work for this are:
--
-- * PlayerCreatureTurn
-- * SnapshotEvent
--
dbRequiresPlanarTurnState :: (DBReadable db) => (CreatureRef -> db a) -> db a
dbRequiresPlanarTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       maybe (throwError $ DBError $ "protocol-error: not in planar turn state (" ++ show state ++ ")") action $ creatureOf state

-- |
-- Perform an action that works only during a player-character's turn.
-- The states that work for this are:
--
-- PlayerCreatureTurn
--
dbRequiresPlayerTurnState :: (DBReadable db) => (CreatureRef -> db a) -> db a
dbRequiresPlayerTurnState action =
    do dbOldestSnapshotOnly
       state <- playerState
       case state of
                  PlayerCreatureTurn creature_ref _ -> action creature_ref
                  _ -> throwError $ DBError $ "protocol-error: not in player turn state (" ++ show state ++ ")"

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

dbDispatchQuery :: (DBReadable db) => [B.ByteString] -> db B.ByteString
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
                           SnapshotEvent (TeleportEvent {}) -> "answer: state teleport-event"
                           SnapshotEvent (HealEvent {}) -> "answer: state heal-event"
                           SnapshotEvent (ClimbEvent {}) -> "answer: state climb-event"
                           SnapshotEvent (ExpendToolEvent {}) -> "answer: state expend-tool-event"
                           GameOver -> "answer: state game-over"

dbDispatchQuery ["action-count"] =
    do n <- dbActionCount
       return $ "answer: action-count " `B.append` (B.pack $ show n)

dbDispatchQuery ["menu-state"] =
    do m_state <- menuState
       return $ case m_state of
           Nothing -> "answer: menu-state 0"
           Just state -> "answer: menu-state " `B.append` (B.pack $ show state)

dbDispatchQuery ["who-attacks"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_source_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
	   SnapshotEvent (MissEvent { miss_event_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
           SnapshotEvent (WeaponOverheatsEvent { weapon_overheats_event_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
           SnapshotEvent (WeaponExplodesEvent { weapon_explodes_event_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
           SnapshotEvent (DisarmEvent { disarm_event_source_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
           SnapshotEvent (SunderEvent { sunder_event_source_creature = attacker_ref }) -> "answer: who-attacks " `B.append` (B.pack $ show $ toUID attacker_ref)
	   _ -> "answer: who-attacks 0"

dbDispatchQuery ["who-hit"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_target_creature = target_ref }) -> "answer: who-hit " `B.append` (B.pack $ show $ toUID target_ref)
           SnapshotEvent (DisarmEvent { disarm_event_target_creature = target_ref }) -> "answer: who-hit " `B.append` (B.pack $ show $ toUID target_ref)
           SnapshotEvent (SunderEvent { sunder_event_target_creature = target_ref }) -> "answer: who-hit " `B.append` (B.pack $ show $ toUID target_ref)
	   _ -> "answer: who-hit 0"

dbDispatchQuery ["tool-used"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (ExpendToolEvent { expend_tool_event_tool = tool_ref }) -> "answer: tool-used " `B.append` (B.pack $ show $ toUID tool_ref)
           _ -> "answer: tool-used 0"

dbDispatchQuery ["weapon-used"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (AttackEvent { attack_event_source_weapon = Just weapon_ref }) -> "answer: weapon-used " `B.append` (B.pack $ show $ toUID weapon_ref)
	   SnapshotEvent (MissEvent { miss_event_weapon = Just weapon_ref }) -> "answer: weapon-used " `B.append` (B.pack $ show $ toUID weapon_ref)
           SnapshotEvent (WeaponOverheatsEvent { weapon_overheats_event_weapon = weapon_ref }) -> "answer: weapon-used " `B.append` (B.pack $ show $ toUID weapon_ref)
           SnapshotEvent (WeaponExplodesEvent { weapon_explodes_event_weapon = weapon_ref }) -> "answer: weapon-used " `B.append` (B.pack $ show $ toUID weapon_ref)
           SnapshotEvent (SunderEvent { sunder_event_source_weapon = weapon_ref }) -> "answer: weapon-used " `B.append` (B.pack $ show $ toUID weapon_ref)
	   _ -> "answer: weapon-used 0"

dbDispatchQuery ["tool-hit"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (DisarmEvent { disarm_event_target_tool = tool_ref }) -> "answer: tool-hit " `B.append` (B.pack $ show $ toUID tool_ref)
           SnapshotEvent (SunderEvent { sunder_event_target_tool = tool_ref }) -> "answer: tool-hit " `B.append` (B.pack $ show $ toUID tool_ref)
           _ -> "answer: tool-hit 0"

dbDispatchQuery ["who-killed"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent (KilledEvent killed_ref) -> "answer: who-killed " `B.append` (B.pack $ show $ toUID killed_ref)
	   _ -> "answer: who-killed 0"

dbDispatchQuery ["who-event"] =
    do state <- playerState
       return $ case state of
           SnapshotEvent event -> "answer: who-event " `B.append` fromMaybe "0" (fmap (B.pack . show . toUID) $ subjectOf event)
           _ -> "answer: who-event 0"

dbDispatchQuery ["player-races","0"] =
    return ("begin-table player-races 0 name\n" `B.append`
	    B.unlines (map B.pack player_race_names) `B.append`
	    "end-table")

dbDispatchQuery ["visible-terrain","0"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       terrain_map <- maybe (return []) (dbGetVisibleTerrainForFaction Player) maybe_plane_ref 
       return ("begin-table visible-terrain 0 x y terrain-type\n" `B.append`
	       (B.unlines $ map (\(terrain_type,Position (x,y)) -> B.unwords $ map B.pack [show x, show y, show terrain_type]) terrain_map) `B.append`
	       "end-table")

dbDispatchQuery ["who-player"] = return "answer: who-player 2"

dbDispatchQuery ["visible-objects","0"] =
    do maybe_plane_ref <- dbGetCurrentPlane
       (objects :: [Location (Reference ()) ()]) <- maybe (return [])
           (dbGetVisibleObjectsForFaction (return . const True) Player) maybe_plane_ref
       table_rows <- mapM (dbObjectToTableRow . child) objects
       return ("begin-table visible-objects 0 object-unique-id x y facing\n" `B.append`
               (B.unlines $ table_rows) `B.append`
               "end-table")
        where dbObjectToTableRow obj_ref =
                do l <- dbWhere obj_ref
                   return $ case (extractParent l,extractParent l) of
                                 (Just (Position (x,y)),maybe_face) -> B.unwords $ map B.pack $ [show $ toUID obj_ref,show x,show y,show $ fromMaybe Here maybe_face]
                                 _ -> ""

dbDispatchQuery ["object-details",uid] = ro $
  do maybe_plane_ref <- dbGetCurrentPlane
     (visibles :: [Reference ()]) <- maybe
         (return [])
         (flip dbGetVisibleObjectsForFaction Player $ \ref ->
              do let f = (== uid) . B.pack . show . toUID
                 let m_wielder = coerceReference ref
                 m_wield <- maybe (return Nothing) dbGetWielded m_wielder
                 return $ maybe False f m_wield || f ref)
         maybe_plane_ref
     let creature_refs = mapMaybe (coerceReferenceTyped _creature) visibles
     wielded <- liftM catMaybes $ mapM dbGetWielded creature_refs
     let tool_refs = mapMaybe (coerceReferenceTyped _tool) visibles ++ wielded
     let building_refs = mapMaybe (coerceReferenceTyped _building) visibles
     creatures <- liftM (zip creature_refs) $ mapRO dbGetCreature creature_refs
     tools <- liftM (zip tool_refs) $ mapRO dbGetTool tool_refs
     buildings <- liftM (zip building_refs) $ mapRO dbGetBuilding building_refs
     liftM B.unlines $ liftM3 (\a b c -> concat [a,b,c])
                            (mapM creatureToTableData creatures)
                            (mapM toolToTableData tools)
                            (mapM buildingToTableData buildings)
   where objectTableWrapper :: (DBReadable db) =>
                               Reference a ->
                               db B.ByteString ->
                               db B.ByteString
         objectTableWrapper obj_ref tableDataF =
          do table_data <- tableDataF
             return $
                 "begin-table object-details " `B.append`
                 (B.pack $ show $ toUID obj_ref) `B.append`
                 " property value\n" `B.append`
                 table_data `B.append`
                 "end-table"
         creatureToTableData :: (DBReadable db) =>
                                (CreatureRef,Creature) ->
                                db B.ByteString
         creatureToTableData (ref,creature) = objectTableWrapper ref $
            do fac <- getCreatureFaction ref
               hp <- getCreatureAbsoluteHealth ref
               maxhp <- getCreatureMaxHealth ref
               return $
                   "object-type creature\n" `B.append`
                   "species " `B.append` (B.pack $ show $ creature_species creature) `B.append` "\n" `B.append`
                   "random-id " `B.append` (B.pack $ show $ creature_random_id creature) `B.append` "\n" `B.append`
                   "faction " `B.append` B.pack (show fac) `B.append` "\n" `B.append`
                       (if fac == Player then
                           "hp " `B.append` B.pack (show hp) `B.append` "\n" `B.append`
                           "maxhp " `B.append` B.pack (show maxhp) `B.append` "\n"
                        else "")
         toolToTableData :: (DBReadable db) => (ToolRef,Tool) -> db B.ByteString
         toolToTableData (ref,tool) = objectTableWrapper ref $ return $
               "object-type tool\n" `B.append`
               "tool-type " `B.append` toolType tool `B.append` "\n" `B.append`
               "tool " `B.append` toolName tool `B.append` "\n"
         buildingToTableData :: (DBReadable db) => (BuildingRef,Building) -> db B.ByteString
         buildingToTableData (ref,Building) = objectTableWrapper ref $
             do building_type <- buildingType ref
                return $ "object-type building\n" `B.append`
                         "building-type " `B.append` B.pack (showBuilding building_type) `B.append` "\n"

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
    do -- constructs a scrolling window of menu items
       -- FIXME!  This should be done client side.
       n <- liftM (fromMaybe 0) menuState
       l <- menuLength
       let half_window = window_size `div` 2
       let window_top = max 0 $ min (l-window_size-1) (n - half_window)
       let windowFilter (x,_,_) = x >= window_top && x <= window_top + window_size
       liftM (showToolMenuTable "menu" s . filter windowFilter) $ toolsToMenuTable =<< toolMenuElements

dbDispatchQuery ["wielded-objects","0"] =
    do m_plane_ref <- dbGetCurrentPlane
       creature_refs <- maybe (return []) (dbGetVisibleObjectsForFaction (return . const True) Player) m_plane_ref
       wielded_tool_refs <- mapM dbGetWielded creature_refs
       let wieldedPairToTable :: CreatureRef -> Maybe ToolRef -> Maybe B.ByteString
           wieldedPairToTable creature_ref = fmap (\tool_ref -> (B.pack $ show $ toUID tool_ref) `B.append` " " `B.append` (B.pack $ show $ toUID creature_ref))
       return $ "begin-table wielded-objects 0 uid creature\n" `B.append`
                B.unlines (catMaybes $ zipWith wieldedPairToTable creature_refs wielded_tool_refs) `B.append`
		"end-table"

dbDispatchQuery ["biome"] =
    do m_plane_ref <- dbGetCurrentPlane
       biome_name <- case m_plane_ref of
           Nothing -> return "nothing"
	   Just plane_ref -> liftM (show . plane_biome) $ dbGetPlane plane_ref
       return $ "answer: biome " `B.append` B.pack biome_name

dbDispatchQuery ["current-plane"] =
    do m_plane_ref <- dbGetCurrentPlane
       return $ case m_plane_ref of
           Nothing -> "answer: current-plane 0"
           Just plane_ref -> "answer: current-plane " `B.append` (B.pack $ show $ toUID plane_ref)

dbDispatchQuery ["plane-random-id"] =
    do m_plane_ref <- dbGetCurrentPlane
       case m_plane_ref of
           Nothing -> return "answer: plane-random-id 0"
           Just plane_ref -> liftM (("answer: plane-random-id " `B.append`) . B.pack . show . plane_random_id) $ dbGetPlane plane_ref

dbDispatchQuery ["planet-name"] =
    do m_plane_ref <- dbGetCurrentPlane
       case m_plane_ref of
         Nothing -> return "answer: planet-name nothing"
         Just plane_ref -> liftM ("answer: planet-name " `B.append`) $ planetName plane_ref

dbDispatchQuery ["compass"] =
    do m_player_ref <- getCurrentCreature Player
       case m_player_ref of
         Nothing -> return "answer: compass nothing"
         Just player_ref -> Perception.runPerception player_ref $ liftM (("answer: compass " `B.append`) . B.pack . show) Perception.compass

dbDispatchQuery ["dungeon-depth"] =
    do m_player_ref <- getCurrentCreature Player
       case m_player_ref of
         Nothing -> return "answer: compass nothing"
         Just player_ref -> Perception.runPerception player_ref $ liftM (("answer: dungeon-depth " `B.append`) . B.pack . show) Perception.depth

dbDispatchQuery unrecognized = return $ "protocol-error: unrecognized query `" `B.append` B.unwords unrecognized `B.append` "`"

-----------------------------------------------------
--      Actions
-----------------------------------------------------

dbDispatchAction :: [B.ByteString] -> DB ()
dbDispatchAction ["continue"] = dbPopOldestSnapshot

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
           _ -> throwError $ DBError $ "protocol-error: not in player turn state"

dbDispatchAction ["normal"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref NormalMode)

dbDispatchAction ["normal",direction] | Just face <- stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> 
        do behavior <- facingBehavior creature_ref face
           dbPerformPlayerTurn behavior creature_ref

dbDispatchAction ["move"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref MoveMode)

dbDispatchAction ["move",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Step $ fromJust $ stringToFacing direction) creature_ref)

dbDispatchAction ["jump"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref JumpMode)

dbDispatchAction ["jump",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState (\creature_ref -> dbPerformPlayerTurn (Behavior.Jump $ fromJust $ stringToFacing direction) creature_ref)

dbDispatchAction ["turn"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref TurnMode)

dbDispatchAction ["turn",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (TurnInPlace $ fromJust $ stringToFacing direction) creature_ref

dbDispatchAction ["clear-terrain"] =
    dbRequiresPlayerTurnState $ \creature_ref -> (setPlayerState $ PlayerCreatureTurn creature_ref ClearTerrainMode)

dbDispatchAction ["clear-terrain",direction] | isJust $ stringToFacing direction =
    dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (ClearTerrain $ fromJust $ stringToFacing direction) creature_ref

dbDispatchAction ["next"] = modifyMenuState (+1)

dbDispatchAction ["prev"] = modifyMenuState (subtract 1)

dbDispatchAction ["select-menu"] =
    do state <- playerState
       i <- menuState
       tool_table <- toolsToMenuTable =<< toolMenuElements
       let selection = maybe "0" (\(_,tool_ref,_) -> B.pack $ show $ toUID tool_ref) $ find (\(n,_,_) -> Just n == i) tool_table
       case state of
           PlayerCreatureTurn _ player_mode -> case player_mode of
               PickupMode {} -> dbDispatchAction ["pickup",selection]
               DropMode {}   -> dbDispatchAction ["drop",selection]
               WieldMode {}  -> dbDispatchAction ["wield",selection]
               MakeMode {}   -> dbDispatchAction ["make-with",selection]
               _ -> throwError $ DBError $ "protocol-error: not in menu selection state"
           _ -> throwError $ DBError $ "protocol-error: not in player turn state"

dbDispatchAction ["make-begin"] = dbRequiresPlayerTurnState $ \creature_ref ->
    setPlayerState (PlayerCreatureTurn creature_ref (MakeMode 0 prepare_make))

dbDispatchAction ["make-what",what] | (Just device_kind) <- readDeviceKind what =
    do state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode n make_prep) -> (setPlayerState $ PlayerCreatureTurn c $ MakeMode n (make_prep `makeWith` device_kind))
           _ -> throwError $ DBError $ "protocol-error: not in make or make-what state"

dbDispatchAction ["make-with",tool_uid] =
    do tool_ref <- readUID ToolRef tool_uid
       tool <- dbGetTool tool_ref
       state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode _ make_prep) -> case (hasChromalite tool, hasMaterial tool, hasGas tool) of
               (Just ch,_,_) | needsChromalite make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (ch,tool_ref))
               (_,Just m,_) | needsMaterial make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (m,tool_ref))
               (_,_,Just g) | needsGas make_prep -> setPlayerState (PlayerCreatureTurn c $ MakeMode 0 $ make_prep `makeWith` (g,tool_ref))
               _ | otherwise -> throwError $ DBError "error: tool doesn't have needed substance"
           _ -> throwError $ DBError "protocol-error: not in make or make-what state"

dbDispatchAction ["make-end"] =
    do state <- playerState
       case state of
           PlayerCreatureTurn c (MakeMode _ make_prep) | isFinished make_prep -> dbPerformPlayerTurn (Make make_prep) c
           PlayerCreatureTurn _ (MakeMode {}) -> throwError $ DBError "protocol-error: make isn't complete"
           _ -> throwError $ DBError "protocol-error: not in make or make-what state"

dbDispatchAction ["pickup"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do pickups <- dbAvailablePickups creature_ref
       case pickups of
           [tool_ref] -> dbPerformPlayerTurn (Pickup tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag NothingAtFeet
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (PickupMode 0))

dbDispatchAction ["pickup",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Pickup tool_ref) creature_ref

dbDispatchAction ["drop"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do inventory <- dbGetContents creature_ref
       case inventory of
           [tool_ref] -> dbPerformPlayerTurn (Drop tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag NothingInInventory
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (DropMode 0))

dbDispatchAction ["drop",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Drop tool_ref) creature_ref

dbDispatchAction ["wield"] = dbRequiresPlayerTurnState $ \creature_ref ->
    do available <- availableWields creature_ref
       case available of
           [tool_ref] -> dbPerformPlayerTurn (Wield tool_ref) creature_ref >> return ()
	   [] -> throwError $ DBErrorFlag NothingInInventory
	   _ -> setPlayerState (PlayerCreatureTurn creature_ref (WieldMode 0))

dbDispatchAction ["wield",tool_uid] = dbRequiresPlayerTurnState $ \creature_ref ->
    do tool_ref <- readUID ToolRef tool_uid
       dbPerformPlayerTurn (Wield tool_ref) creature_ref

dbDispatchAction ["unwield"] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn Unwield creature_ref

dbDispatchAction ["fire"] =
    dbRequiresPlayerTurnState $ \creature_ref -> rangedAttackModel creature_ref >> setPlayerState (PlayerCreatureTurn creature_ref FireMode)

dbDispatchAction ["fire",direction] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (Fire $ fromJust $ stringToFacing direction) creature_ref

dbDispatchAction ["attack"] =
    dbRequiresPlayerTurnState $ \creature_ref -> meleeAttackModel creature_ref >> setPlayerState (PlayerCreatureTurn creature_ref AttackMode)

dbDispatchAction ["attack",direction] = dbRequiresPlayerTurnState $ \creature_ref -> dbPerformPlayerTurn (Attack $ fromJust $ stringToFacing direction) creature_ref

dbDispatchAction ["activate"] = dbRequiresPlayerTurnState $ dbPerformPlayerTurn Activate

dbDispatchAction ["down"] =
    dbRequiresPlayerTurnState $ dbPerformPlayerTurn StepDown

dbDispatchAction ["up"] =
    dbRequiresPlayerTurnState $ dbPerformPlayerTurn StepUp

dbDispatchAction unrecognized = throwError $ DBError $ ("protocol-error: unrecognized action `" ++ (B.unpack $ B.unwords unrecognized) ++ "`")

dbSelectPlayerRace :: B.ByteString -> DB ()
dbSelectPlayerRace race_name =
    case find (\s -> B.map toLower (B.pack $ show s) == race_name) player_species of
        Nothing -> throwError $ DBError $ "protocol-error: unrecognized race '" ++ B.unpack race_name ++ "'"
        Just species -> generateInitialPlayerCreature species

dbSelectPlayerClass :: B.ByteString -> Creature -> DB ()
dbSelectPlayerClass class_name creature =
    let eligable_base_classes = getEligableBaseCharacterClasses creature
	in case find (\x -> (B.map toLower . B.pack . show) x == class_name) eligable_base_classes of
	       Nothing -> throwError $ DBError $ "protocol-error: unrecognized or invalid class '" ++ B.unpack class_name ++ "'"
	       Just the_class -> dbBeginGame creature the_class

dbRerollRace :: Creature -> DB ()
dbRerollRace _ = do starting_race <- dbGetStartingRace
		    generateInitialPlayerCreature $ fromJust starting_race

dbQueryPlayerStats :: (DBReadable db) => Creature -> db B.ByteString
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
toolsToMenuTable :: (DBReadable db) => [ToolRef] -> db [(Integer,ToolRef,B.ByteString)]
toolsToMenuTable raw_uids =
    do let uids = sortBy (comparing toUID) raw_uids
       tool_names <- mapM (liftM toolName . dbGetTool) uids
       return $ zip3 [0..] uids tool_names

menuLength :: (DBReadable db) => db Integer
menuLength = liftM genericLength toolMenuElements

-- |
-- Generate a tool menu table in text form, with the specified name and element list.
--
showToolMenuTable :: B.ByteString -> B.ByteString -> [(Integer,ToolRef,B.ByteString)] -> B.ByteString
showToolMenuTable table_name table_id tool_table = 
    "begin-table " `B.append` table_name `B.append` " " `B.append` table_id `B.append` " n uid name" `B.append` "\n" `B.append`
    B.unlines (map (\(n,uid,tool_name) -> B.unwords [B.pack $ show n,B.pack $ show $ toUID uid,tool_name]) tool_table) `B.append`
    "end-table"

-- |
-- Information about player creatures (for which the player should have almost all available information.)
--
playerStatsTable :: Creature -> B.ByteString
playerStatsTable c =
    "begin-table player-stats 0 property value\n" `B.append`
               "str " `B.append` (B.pack $ show $ rawScore Strength c) `B.append` "\n" `B.append`
	       "spd " `B.append` (B.pack $ show $ rawScore Speed c) `B.append` "\n" `B.append`
	       "con " `B.append` (B.pack $ show $ rawScore Constitution c) `B.append` "\n" `B.append`
	       "int " `B.append` (B.pack $ show $ rawScore Intellect c) `B.append` "\n" `B.append`
	       "per " `B.append` (B.pack $ show $ rawScore Perception c) `B.append` "\n" `B.append`
	       "cha " `B.append` (B.pack $ show $ rawScore Charisma c) `B.append` "\n" `B.append`
	       "mind " `B.append` (B.pack $ show $ rawScore Mindfulness c) `B.append` "\n" `B.append`
	       "maxhp " `B.append` (B.pack $ show $ creatureAbilityScore ToughnessTrait c) `B.append` "\n" `B.append`
	       "species " `B.append` (B.pack $ show $ creature_species c) `B.append` "\n" `B.append`
	       "random-id " `B.append` (B.pack $ show $ creature_random_id c) `B.append` "\n" `B.append`
	       "gender " `B.append` (B.pack $ show $ creatureGender c) `B.append` "\n" `B.append`
	       "end-table"

toolName :: Tool -> B.ByteString
toolName (DeviceTool _ d) = deviceName d
toolName (Sphere s) = prettySubstance s

toolType :: Tool -> B.ByteString
toolType (DeviceTool Gun _) = "gun"
toolType (DeviceTool Sword _) = "sword"
toolType (Sphere (GasSubstance _)) = "sphere-gas"
toolType (Sphere (MaterialSubstance _)) = "sphere-material"
toolType (Sphere (ChromaliteSubstance _)) = "sphere-chromalite"

dbQueryBaseClasses :: (DBReadable db) => Creature -> db B.ByteString
dbQueryBaseClasses creature = return $ baseClassesTable creature

baseClassesTable :: Creature -> B.ByteString
baseClassesTable creature = 
    "begin-table base-classes 0 class\n" `B.append`
    (B.unlines $ map (B.pack . show) $ getEligableBaseCharacterClasses creature) `B.append`
    "end-table"

dbQueryCenterCoordinates :: (DBReadable db) => CreatureRef -> db B.ByteString
dbQueryCenterCoordinates creature_ref =
    do l <- dbWhere creature_ref
       case (extractParent l,extractParent l :: Maybe Facing) of
		(Just (Position (x,y)),Nothing) -> 
                    return (begin_table `B.append`
			    "x " `B.append` B.pack (show x) `B.append` "\n" `B.append`
			    "y " `B.append` B.pack (show y) `B.append` "\n" `B.append`
			    "end-table")
                (Just (Position (x,y)),Just face) -> 
                    return (begin_table `B.append`
                           "x " `B.append` B.pack (show x) `B.append` "\n" `B.append`
                           "y " `B.append` B.pack (show y) `B.append` "\n" `B.append`
                           "facing " `B.append` B.pack (show face) `B.append` "\n" `B.append`
                           "end-table")
		_ -> return (begin_table `B.append` "end-table")
	   where begin_table = "begin-table center-coordinates 0 axis coordinate\n"

readUID :: (Integer -> Reference a) -> B.ByteString -> DB (Reference a)
readUID f x = 
    do let m_uid = readNumber x
       ok <- maybe (return False) (dbVerify . f) m_uid
       when (not ok) $ throwError $ DBError $ "protocol-error: " ++ B.unpack x ++ " is not a valid uid."
       return $ f $ fromJust m_uid

readNumber :: B.ByteString -> Maybe Integer
readNumber = fmap fst . B.readInteger

readDeviceKind :: B.ByteString -> Maybe DeviceKind
readDeviceKind "pistol" = Just Pistol
readDeviceKind "carbine" = Just Carbine
readDeviceKind "rifle" = Just Rifle
readDeviceKind "fleuret" = Just Fleuret
readDeviceKind "sabre" = Just Sabre
readDeviceKind _ = Nothing
