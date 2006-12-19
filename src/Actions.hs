module Actions
    (takeUserInputAction,
     getValidActions)
    where

import System.Exit
import Control.Monad
import Driver
import Data.IORef
import Data.List
import Globals
import PrintText
import Tables
import Data.Maybe
import System.IO

data Action = Action { action_valid :: IORef RoguestarGlobals -> IO Bool, 
		       action_execute :: IORef RoguestarGlobals -> IO () }

quitAction :: (String,Action)
quitAction = ("quit",
              Action { action_valid = \_ -> return True,
                       action_execute = \_ -> exitWith ExitSuccess })

selectRaceAction :: String -> Action
selectRaceAction = selectTableAction ("player-races","0","name") "race-selection" "select-race"

selectBaseClassAction :: String -> Action
selectBaseClassAction = selectTableAction ("base-classes","0","class") "class-selection" "select-class"

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
selectTableAction :: (String,String,String) -> String -> String -> String -> Action
selectTableAction (the_table_name,the_table_id,the_table_header) allowed_state action_name action_param = 
    Action { action_valid = valid,
             action_execute = execute }
    where valid globals_ref = 
	      do state <- driverGetAnswer globals_ref Fresh "state"
                 case state of 
                            Nothing -> return False
		            Just x | x == allowed_state -> 
		                do maybe_table <- driverGetTable globals_ref Fresh the_table_name the_table_id
		                   case maybe_table of 
		                                    Nothing -> return False
		                                    Just table -> return $ action_param `elem` tableSelect1 table the_table_header
		            Just _ -> return False
	  execute globals_ref = 
	      do driverAction globals_ref [action_name, action_param]
	         printTranslated globals_ref GUIMessage ["table-action",action_name,action_param]

moveAction :: String -> (String,Action)
moveAction str =
    ("move-" ++ str,
     Action {
	     action_valid = \globals_ref -> liftM (== Just "player-turn") $ driverGetAnswer globals_ref Fresh "state",
	     action_execute = \globals_ref -> driverAction globals_ref ["move",str]
	    })

reroll_action :: (String,Action)
reroll_action =
    ("reroll",
     Action {
	     action_valid = \globals_ref -> liftM (== Just "class-selection") $ driverGetAnswer globals_ref Fresh "state",
	     action_execute = \globals_ref -> driverAction globals_ref ["reroll"]
            })

select_race_action_names :: [String]
select_race_action_names = ["anachronid",
			    "androsynth",
			    "ascendant",
			    "caduceator",
			    "encephalon",
			    "goliath",
			    "hellion",
			    "kraken",
			    "myrmidon",
			    "perennial",
			    "recreant",
			    "reptilian"]

select_base_class_action_names :: [String]
select_base_class_action_names = ["barbarian",
				  "consular",
				  "engineer",
				  "forceadept",
				  "marine",
				  "ninja",
				  "pilot",
				  "privateer",
				  "scout",
				  "shepherd",
				  "thief",
				  "warrior"]

eight_directions :: [String]
eight_directions = ["n","ne","nw","e","w","se","sw","s"]

select_race_actions :: [(String,Action)]
select_race_actions = map (\x -> (x,selectRaceAction x)) select_race_action_names

select_base_class_actions :: [(String,Action)]
select_base_class_actions = map (\x -> (x,selectBaseClassAction x)) select_base_class_action_names

move_actions :: [(String,Action)]
move_actions = map moveAction eight_directions

all_actions :: [(String,Action)]
all_actions = [quitAction] ++
              select_race_actions ++ 
	      [reroll_action] ++
	      select_base_class_actions ++
	      move_actions

-- |
-- Answer a complete list of all actions whose action_valid functions answer True.
--
getValidActions :: IORef RoguestarGlobals -> IO [String]
getValidActions globals_ref = 
    do valid_action_pairs <- filterM ((\x -> action_valid x globals_ref) . snd) all_actions
       return $ map fst valid_action_pairs

-- |
-- Takes a list of action names and executes the first action in the list that is valid
-- in the current context, based on each action's action_valid entry.
-- Returns True if an action was taken, False otherwise.
--
takeUserInputAction :: IORef RoguestarGlobals -> [String] -> IO Bool
takeUserInputAction _ [] = return False
takeUserInputAction globals_ref action_names = 
    do valid_actions <- getValidActions globals_ref
       let single_valid_action = intersect action_names valid_actions
	   executeAction = do action_execute (fromJust $ lookup (head single_valid_action) all_actions) globals_ref
			      return True
       case length single_valid_action of
				       0 -> return False
				       1 -> executeAction
				       _ -> do printTranslated globals_ref GUIMessage (["action-bindings-warning"] ++ single_valid_action)
					       executeAction
