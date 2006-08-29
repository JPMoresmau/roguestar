module Actions
    (takeUserInputAction,
     getValidActions)
    where

import Control.Monad
import Driver
import Data.IORef
import Data.List
import Globals
import PrintText
import Tables
import Data.Maybe

data Action = Action { action_valid :: IORef RoguestarGlobals -> IO Bool, 
		       action_execute :: IORef RoguestarGlobals -> IO () }

selectRaceAction :: String -> Action
selectRaceAction race_name = 
    Action {
	    action_valid = \globals_ref -> 
	    do {
		state <- driverRequestAnswer globals_ref "state";
		case state of 
		{
		 Nothing -> return False;
		 Just "race-selection" -> 
		 do { 
		     maybe_table <- driverRequestTable globals_ref "player-races" "0";
		     case maybe_table of 
		     {
		      Nothing -> return False;
		      Just table -> return $ race_name `elem` tableSelect1 table "name"
		     }};
		 Just _ -> return False
		}},

	    action_execute = \globals_ref -> do {
						 driverAction globals_ref ["select-race", race_name];
						 printTranslated globals_ref GUIMessage ["user-selected-species",race_name]
						}
	   }

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

select_race_actions :: [(String,Action)]
select_race_actions = map (\x -> (x,selectRaceAction x)) select_race_action_names

all_actions :: [(String,Action)]
all_actions = select_race_actions

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
