{-# LANGUAGE OverloadedStrings #-}
-- |
-- A set of actions that should be bound to keystrokes.
--
module Actions
    (takeUserInputAction,
     getValidActions,
     ActionInput(..),
     select_race_action_names,
     select_base_class_action_names,
     make_what_action_names,
     executeContinueAction,
     menu_states,
     player_turn_states)
    where

import Control.Monad.Error
import Driver
import Data.List
import PrintText
import Tables
import Data.Maybe
import Globals
import RSAGL.Math.Types
import Control.Concurrent.STM
import Quality
import Data.Char
import qualified Data.ByteString.Char8 as B

-- |
-- Input to an action.
--
data ActionInput = ActionInput {
    action_globals :: Globals,
    action_driver_object :: DriverObject,
    action_print_text_object :: PrintTextObject }

type Action = ActionInput -> ErrorT ActionValidityReason STM (STM ())

data ActionValidity = Go | Hold ActionValidityReason deriving (Read, Show)
data ActionValidityReason = Invalid | Undecided String deriving (Read,Show)

instance Error ActionValidityReason where
    noMsg = strMsg ""
    strMsg = Undecided

invalid :: ErrorT ActionValidityReason STM ()
invalid = throwError Invalid

isUndecided :: ActionValidity -> Bool
isUndecided (Hold (Undecided _)) = True
isUndecided _ = False

isGo :: ActionValidity -> Bool
isGo Go = True
isGo _ = False

{----------------------------------------------------------
 --  Support functions for Actions
 ----------------------------------------------------------}

actionValid :: ActionInput -> Action -> STM ActionValidity
actionValid action_input action = 
    do result <- runErrorT $ action action_input
       return $ either Hold (const Go) result

executeAction :: ActionInput -> Action -> STM ()
executeAction action_input action =
    do result <- runErrorT $ action action_input
       either (\failure -> printText
                           (action_print_text_object action_input)
                           UnexpectedEvent
                           ("unable to execute action: " `B.append`
                           B.pack (show failure)))
              (id)
              result
       return ()

executeContinueAction :: ActionInput -> STM ()
executeContinueAction action_input =
    do either (const $ return()) id =<< (runErrorT $ (snd continue_action) action_input)
       return ()

-- |
-- Constructs an action such that the action is valid only if it is listed in
-- a table from the engine.  The first parameter identifies the table column to look up,
-- (table name, table id, header to look under).  The second parameter is the name of the
-- action as it will be send to the engine.  The third parameter is the action_param.
-- The action_param must be listed under the specified header of the specified table,
-- or the action will not be allowed to execute.
--
-- For example: selectTableAction ("people","0","name") "select-person-to-call-state" "call-with-telephone" "carl"
-- 
-- \> game query state
-- answer: state select-person-to-call-state
-- \> game query people
-- begin-table people 0 name
-- john
-- wendy
-- carl
-- bob
-- end-table
-- \> game action call-with-telephone Carl
-- 
-- In this case the action executes because the state is actually select-person-to-call-state and Carl
-- is actually listed under the "name" header of the "people" table.
--
-- In practice this function is used for things like the race-selection-state and the class-selection-state
-- where we select from a predifined list of possible choices, but the engine further restricts the choices.
-- Each possible choice is it's own action that will only return action_valid if it is listed in the appropriate
-- table from the engine. 
--
selectTableAction :: (B.ByteString,B.ByteString,B.ByteString) -> B.ByteString -> B.ByteString -> B.ByteString -> Action
selectTableAction (the_table_name,the_table_id,the_table_header) allowed_state action_name action_param = stateGuard [allowed_state] $
    \action_input ->
        do table <- maybe (fail $ "need table " ++ B.unpack the_table_name) return =<< (lift $ getTable (action_driver_object action_input) the_table_name the_table_id)
           unless ([action_param] `elem` tableSelect table [the_table_header]) invalid
           return $ driverAction (action_driver_object action_input) [action_name, action_param]

-- |
-- Guard an action to only run during specific states.
--
stateGuard :: [B.ByteString] -> Action -> Action
stateGuard allowed_states actionM action_input =
    do is_valid_state <- liftM (maybe False (`elem` allowed_states)) $ lift $ getAnswer (action_driver_object action_input) "state"
       unless is_valid_state invalid
       actionM action_input

-- |
-- A simple action that can run whenever the player state is a member of a given list.
-- The action name is passed directly to the engine.
--
stateLinkedAction :: [B.ByteString] -> B.ByteString -> (B.ByteString,Action)
stateLinkedAction allowed_state action_name = 
    (action_name,
     stateGuard allowed_state $ \action_input ->
         return $ driverAction (action_driver_object action_input) [action_name])

-- |
-- An action that can always run, like "quit".
--
alwaysAction :: B.ByteString -> (ActionInput -> STM ()) -> (B.ByteString,Action)
alwaysAction action_name actionSTM =
    (action_name,
     \action_input -> return $ actionSTM action_input)

{----------------------------------------------------------
    Specific Actions
 ----------------------------------------------------------}

player_turn_states :: [B.ByteString]
player_turn_states = ["player-turn","move","attack","fire","jump","turn","clear-terrain"]

menu_states :: [B.ByteString]
menu_states = ["race-selection","class-selection","pickup","drop","wield","make","make-finished","make-what"]

selectable_menu_states :: [B.ByteString]
selectable_menu_states = if all (`elem` menu_states) states then states else error "selectable_menu_states: inconsistent with menu_states"
    where states = ["pickup","drop","wield","make"]

quit_action :: (B.ByteString,Action)
quit_action = alwaysAction "quit" $ \action_input ->
    writeTVar (global_should_quit $ action_globals action_input) True

continue_action :: (B.ByteString,Action)
continue_action = ("continue",\action_input ->
    do is_snapshot <- liftM (== Just "yes") $ lift $ getAnswer (action_driver_object action_input) "snapshot"
       unless is_snapshot invalid
       return $ driverAction (action_driver_object action_input) ["continue"])

direction_actions :: [(B.ByteString,Action)]
direction_actions = map (stateLinkedAction player_turn_states) ["n","ne","e","se","s","sw","w","nw"]

next_action :: (B.ByteString,Action)
next_action = stateLinkedAction selectable_menu_states "next"

prev_action :: (B.ByteString,Action)
prev_action = stateLinkedAction selectable_menu_states "prev"

select_menu_action :: (B.ByteString,Action)
select_menu_action = stateLinkedAction selectable_menu_states "select-menu"

normal_action :: (B.ByteString,Action)
normal_action = ("normal",
    stateGuard (menu_states ++ player_turn_states) $ \action_input -> 
        return $ driverAction (action_driver_object action_input) ["normal"])

move_action :: (B.ByteString,Action)
move_action = stateLinkedAction player_turn_states "move"

down_action :: (B.ByteString,Action)
down_action = stateLinkedAction player_turn_states "down"

jump_action :: (B.ByteString,Action)
jump_action = stateLinkedAction player_turn_states "jump"

turn_action :: (B.ByteString,Action)
turn_action = stateLinkedAction player_turn_states "turn"

fire_action :: (B.ByteString,Action)
fire_action = stateLinkedAction player_turn_states "fire"

attack_action :: (B.ByteString,Action)
attack_action = stateLinkedAction player_turn_states "attack"

clear_terrain_action :: (B.ByteString,Action)
clear_terrain_action = stateLinkedAction player_turn_states "clear-terrain"

reroll_action :: (B.ByteString,Action)
reroll_action = stateLinkedAction ["class-selection"] "reroll"

pickup_action :: (B.ByteString,Action)
pickup_action = stateLinkedAction player_turn_states "pickup"

drop_action :: (B.ByteString,Action)
drop_action = stateLinkedAction player_turn_states "drop"

wield_action :: (B.ByteString,Action)
wield_action = stateLinkedAction player_turn_states "wield"

unwield_action :: (B.ByteString,Action)
unwield_action = stateLinkedAction player_turn_states "unwield"

activate_action :: (B.ByteString,Action)
activate_action = stateLinkedAction player_turn_states "activate"

make_begin_action :: (B.ByteString,Action)
make_begin_action = stateLinkedAction player_turn_states "make-begin"

make_what_action_names :: [B.ByteString]
make_what_action_names = ["pistol","carbine","rifle","fleuret","sabre"]

makeWhatAction :: B.ByteString -> (B.ByteString,Action)
makeWhatAction s = (s,
    stateGuard ["make-what","make","make-finished"] $ \action_input ->
        return $ driverAction (action_driver_object action_input) ["make-what",s])

make_what_actions :: [(B.ByteString,Action)]
make_what_actions = map makeWhatAction make_what_action_names

make_end_action :: (B.ByteString,Action)
make_end_action = stateLinkedAction ["make-finished"] "make-end"

selectRaceAction :: B.ByteString -> (B.ByteString,Action)
selectRaceAction s =
    (s,selectTableAction ("player-races","0","name") "race-selection" "select-race" s)

selectBaseClassAction :: B.ByteString -> (B.ByteString,Action)
selectBaseClassAction s =
    (s,selectTableAction ("base-classes","0","class") "class-selection" "select-class" s)

zoomSize :: RSdouble -> RSdouble
zoomSize x | x < 3 = 0.2
zoomSize x | x < 10 = 1.0
zoomSize _ | otherwise = 5.0

zoomIn :: RSdouble -> RSdouble
zoomIn x = x - (zoomSize $ x - zoomSize x)

zoomOut :: RSdouble -> RSdouble
zoomOut x = x + (zoomSize x)

zoom_in_action :: (B.ByteString,Action)
zoom_in_action = alwaysAction "zoom-in" $ \action_input ->
    do writeTVar (global_planar_camera_distance $ action_globals action_input) .
           (max 1.0 . zoomIn) =<< readTVar (global_planar_camera_distance $
                                                action_globals action_input)
       return ()

zoom_out_action :: (B.ByteString,Action)
zoom_out_action = alwaysAction "zoom-out" $ \action_input ->
    do writeTVar (global_planar_camera_distance $ action_globals action_input) .
           (min 25.0 . zoomOut) =<< readTVar (global_planar_camera_distance $
                                                action_globals action_input)
       return ()

sky_on_action :: (B.ByteString,Action)
sky_on_action = alwaysAction "sky-on" $ \action_input ->
    do writeTVar (global_sky_on $ action_globals action_input) True
       return ()

sky_off_action :: (B.ByteString,Action)
sky_off_action = alwaysAction "sky-off" $ \action_input ->
    do writeTVar (global_sky_on $ action_globals action_input) False
       return ()

setQualityAction :: Quality -> (B.ByteString,Action)
setQualityAction q =
    alwaysAction (B.pack $ map toLower $ "quality-" ++ show q) $
           \action_input ->
        do writeTVar (global_quality_setting $ action_globals action_input) q
           return ()

quality_bad :: (B.ByteString,Action)
quality_bad = setQualityAction Bad

quality_poor :: (B.ByteString,Action)
quality_poor = setQualityAction Poor

quality_good :: (B.ByteString,Action)
quality_good = setQualityAction Good

quality_super :: (B.ByteString,Action)
quality_super = setQualityAction Super

{----------------------------------------------------------
    Lists of Known Actions
 ----------------------------------------------------------}

select_race_action_names :: [B.ByteString]
select_race_action_names = [--"anachronid",
			    "androsynth",
			    "ascendant",
			    "caduceator",
			    "encephalon",
			    --"goliath",
			    --"hellion",
			    --"kraken",
			    --"myrmidon",
			    --"perennial",
			    "recreant",
			    "reptilian"]

select_race_actions :: [(B.ByteString,Action)]
select_race_actions = map selectRaceAction select_race_action_names

select_base_class_action_names :: [B.ByteString]
select_base_class_action_names = ["barbarian",
				  "consular",
				  "engineer",
				  "forceadept",
				  "marine",
				  "ninja",
				  "pirate",
				  "scout",
				  "shepherd",
				  "thief",
				  "warrior"]

select_base_class_actions :: [(B.ByteString,Action)]
select_base_class_actions = map selectBaseClassAction select_base_class_action_names

-- | List of every action in the game.
all_actions :: [(B.ByteString,Action)]
all_actions = [continue_action,quit_action,reroll_action,
               pickup_action,drop_action,wield_action,unwield_action,
               next_action,prev_action,normal_action,select_menu_action,
               zoom_in_action,zoom_out_action,sky_on_action,sky_off_action,
               quality_bad,quality_poor,quality_good,quality_super] ++
              select_race_actions ++
	      select_base_class_actions ++
              direction_actions ++
              make_what_actions ++
	      [move_action,down_action,turn_action,fire_action,jump_action,
               attack_action,clear_terrain_action,activate_action,
               make_begin_action,make_end_action]

-- | Find an action with the given name.
lookupAction :: B.ByteString -> (B.ByteString,Action)
lookupAction x = (x,fromMaybe (error $ "tried to operate on an unknown action named " ++ B.unpack x) $ lookup x all_actions)

-- |
-- Answer a complete list of all actions that can be run at this time.
-- Accepts an optional list of action names to choose from; if Nothing,
-- uses all concievable actions as that list.
--
getValidActions :: ActionInput -> Maybe [B.ByteString] -> STM [B.ByteString]
getValidActions action_input actions_list =
    do valid_action_pairs <- mapM (\a -> liftM ((,) a) $ actionValid action_input $ snd a) $ maybe all_actions (map lookupAction) actions_list
       return $ case () of
           () | any (isUndecided . snd) valid_action_pairs -> []
           () | otherwise -> map (fst . fst) $ filter (isGo . snd) valid_action_pairs

-- |
-- Takes a list of action names and executes the first action in the list that is valid
-- in the current context, based on each action's action_valid entry.
-- Returns True if an action was taken, False otherwise.
--
takeUserInputAction :: ActionInput -> [B.ByteString] -> STM Bool
takeUserInputAction _ [] = return False
takeUserInputAction action_input action_names =
    do valid_actions <- getValidActions action_input (Just action_names)
       let action = map lookupAction valid_actions
       case length action of
           0 -> return False
           1 -> do executeAction action_input $ snd $ head action
                   return True
           _ -> do driverSendError (action_driver_object action_input)
                       ("Action bindings warning: multiple valid action for binding: " `B.append`
                       (B.concat $ intersperse ", " $ map fst action))
                   executeAction action_input $ snd $ head action
                   return True

