--------------------------------------------------------------------------
--  roguestar-gl: the space-adventure roleplaying game OpenGL frontend.   
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

module Actions
    (takeUserInputAction,
     getValidActions)
    where

import System.Exit
import Control.Monad
import Control.Monad.Error
import Driver
import Data.IORef
import Data.List
import Globals
import PrintText
import Tables
import Data.Maybe
import System.IO

type Action = IORef RoguestarGlobals -> ErrorT String IO (IO ())

actionValid :: IORef RoguestarGlobals -> Action -> IO Bool
actionValid globals_ref action = 
    do result <- runErrorT $ action globals_ref
       return $ either (const False) (const True) result

executeAction :: IORef RoguestarGlobals -> Action -> IO ()
executeAction globals_ref action =
    do result <- runErrorT $ action globals_ref
       either (\s -> printText globals_ref Untranslated ("unable to execute action: " ++ s))
              (id)
              result
       return ()

quit_action :: (String,Action)
quit_action = ("quit",
               \_ -> return $ exitWith ExitSuccess)

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
    \globals_ref ->
        do state <- maybe (fail "") return =<< (lift $ driverGetAnswer globals_ref Fresh "state")
           guard $ state == allowed_state
           table <- maybe (fail "") return =<< (lift $ driverGetTable globals_ref Fresh the_table_name the_table_id)
           guard $ action_param `elem` tableSelect1 table the_table_header
           return $ do driverAction globals_ref [action_name, action_param]
                       printTranslated globals_ref GUIMessage ["table-action",action_name,action_param]

-- |
-- An action that depends on the state of the game and an arbitrary constant parameter.
-- For example, actions that operate directionally are parameterized
-- on the eight directions (n,s,w,e,nw,ne,sw,se).
--
-- i.e., parameterizedAction "player-turn" "move" "nw" becomes 
-- ("move-nw",driverAction _ ["move","nw"])
--
parameterizedAction :: String -> String -> String -> (String,Action)
parameterizedAction allowed_state action_name parameter =
    (action_name ++ "-" ++ parameter,
     \globals_ref ->
         do guard =<< (liftM (== Just allowed_state) $ lift $ driverGetAnswer globals_ref Fresh "state")
            return $ driverAction globals_ref [action_name,parameter])

moveAction :: String -> (String,Action)
moveAction = parameterizedAction "player-turn" "move"

turnAction :: String -> (String,Action)
turnAction = parameterizedAction "player-turn" "turn"

reroll_action :: (String,Action)
reroll_action =
    ("reroll",
     \globals_ref ->
         do guard =<< (liftM (== Just "class-selection") $ lift $ driverGetAnswer globals_ref Fresh "state")
            return $ driverAction globals_ref ["reroll"])

selectRaceAction :: String -> (String,Action)
selectRaceAction s = 
    (s,selectTableAction ("player-races","0","name") "race-selection" "select-race" s)

selectBaseClassAction :: String -> (String,Action)
selectBaseClassAction s = 
    (s,selectTableAction ("base-classes","0","class") "class-selection" "select-class" s)

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
select_race_actions = map selectRaceAction select_race_action_names

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

select_base_class_actions :: [(String,Action)]
select_base_class_actions = map selectBaseClassAction select_base_class_action_names

eight_directions :: [String]
eight_directions = ["n","ne","nw","e","w","se","sw","s"]

move_actions :: [(String,Action)]
move_actions = map moveAction eight_directions

turn_actions :: [(String,Action)]
turn_actions = map turnAction eight_directions

all_actions :: [(String,Action)]
all_actions = [quit_action] ++
              select_race_actions ++ 
	      [reroll_action] ++
	      select_base_class_actions ++
	      move_actions ++
              turn_actions

lookupAction :: String -> (String,Action)
lookupAction x = (x,fromMaybe (error $ "tried to operate on an unknown action named " ++ x) $ lookup x all_actions)

-- |
-- Answer a complete list of all actions that can be run at this time.
-- Accepts an optional list of action names to choose from; if Nothing,
-- uses all concievable actions as that list.
--
getValidActions :: IORef RoguestarGlobals -> Maybe [String] -> IO [String]
getValidActions globals_ref actions_list = 
    do valid_action_pairs <- filterM (actionValid globals_ref . snd) $ 
                                 maybe all_actions (map lookupAction) actions_list
       return $ map fst valid_action_pairs

-- |
-- Takes a list of action names and executes the first action in the list that is valid
-- in the current context, based on each action's action_valid entry.
-- Returns True if an action was taken, False otherwise.
--
takeUserInputAction :: IORef RoguestarGlobals -> [String] -> IO Bool
takeUserInputAction _ [] = return False
takeUserInputAction globals_ref action_names =
    do valid_actions <- getValidActions globals_ref (Just action_names)
       let action = map lookupAction valid_actions
       case length action of
           0 -> return False
	   1 -> do executeAction globals_ref $ snd $ head action
                   return True
	   _ -> do printTranslated globals_ref GUIMessage (["action-bindings-warning"] ++ map fst action)
		   executeAction globals_ref $ snd $ head action
                   return True
