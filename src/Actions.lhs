
\section{Actions}

\begin{code}
module Actions
    (takeUserInputAction,
     getValidActions,
     ActionInput(..),
     select_race_action_names,
     select_base_class_action_names,
     executeContinueAction)
    where

import System.Exit
import Control.Monad.Error
import Driver
import Data.List
import PrintText
import Tables
import Data.Maybe
import System.IO
import Globals
import Data.IORef

-- |
-- Input to an action.
--
data ActionInput = ActionInput {
    action_globals :: IORef Globals,
    action_driver_object :: DriverObject,
    action_print_text_object :: PrintTextObject }

type Action = ActionInput -> ErrorT String IO (IO ())

{----------------------------------------------------------
    Supports functions for Actions
 ----------------------------------------------------------}

actionValid :: ActionInput -> Action -> IO Bool
actionValid action_input action = 
    do result <- runErrorT $ action action_input
       return $ either (const False) (const True) result

executeAction :: ActionInput -> Action -> IO ()
executeAction action_input action =
    do result <- runErrorT $ action action_input
       either (\s -> printText (action_print_text_object action_input) UnexpectedEvent ("unable to execute action: " ++ s))
              (id)
              result
       return ()

executeContinueAction :: ActionInput -> IO ()
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
selectTableAction :: (String,String,String) -> String -> String -> String -> Action
selectTableAction (the_table_name,the_table_id,the_table_header) allowed_state action_name action_param = 
    \action_input ->
        do state <- maybe (fail "") return =<< (lift $ getAnswer (action_driver_object action_input) "state")
           guard $ state == allowed_state
           table <- maybe (fail "") return =<< (lift $ getTable (action_driver_object action_input) the_table_name the_table_id)
           guard $ [action_param] `elem` tableSelect table [the_table_header]
           return $ driverAction (action_driver_object action_input) [action_name, action_param]

-- |
-- An action that depends on the state flag of the game engine and an arbitrary constant parameter.
-- For example, actions that operate directionally are parameterized
-- on the eight directions (n,s,w,e,nw,ne,sw,se).
--
-- i.e., parameterizedAction "player-turn" "move" "nw" becomes 
-- ("move-nw",driverAction _ ["move","nw"])
--
parameterizedAction :: String -> String -> String -> (String,Action)
parameterizedAction allowed_state action_name parameter =
    (action_name ++ "-" ++ parameter,
     stateGuard allowed_state $ \action_input ->
         return $ driverAction (action_driver_object action_input) [action_name,parameter])

-- |
-- Guard an action to only run during a specific state.
--
stateGuard :: String -> Action -> Action
stateGuard allowed_state actionM action_input =
    do guard =<< (liftM (== Just allowed_state) $ lift $ getAnswer (action_driver_object action_input) "state")
       actionM action_input

-- |
-- An action that depends just on the state flag of the game engine.
--
stateLinkedAction :: String -> String -> (String,Action)
stateLinkedAction allowed_state action_name = 
    (action_name,
     stateGuard allowed_state $ \action_input ->
         return $ driverAction (action_driver_object action_input) [action_name])

-- |
-- An action that can always run, like "quit".
--
alwaysAction :: String -> (ActionInput -> IO ()) -> (String,Action)
alwaysAction action_name actionIO =
    (action_name,
     \action_input -> return $ actionIO action_input)

{----------------------------------------------------------
    Specific Actions
 ----------------------------------------------------------}

quit_action :: (String,Action)
quit_action = alwaysAction "quit" $ \_ -> exitWith ExitSuccess

continue_action :: (String,Action)
continue_action = ("continue",\action_input ->
    do guard =<< (liftM (== Just "yes") $ lift $ getAnswer (action_driver_object action_input) "snapshot")
       return $ driverAction (action_driver_object action_input) ["continue"])

moveAction :: String -> (String,Action)
moveAction = parameterizedAction "player-turn" "move"

turnAction :: String -> (String,Action)
turnAction = parameterizedAction "player-turn" "turn"

fireAction :: String -> (String,Action)
fireAction = parameterizedAction "player-turn" "fire"

reroll_action :: (String,Action)
reroll_action = stateLinkedAction "class-selection" "reroll"

pickup_action :: (String,Action)
pickup_action = stateLinkedAction "player-turn" "pickup"

drop_action :: (String,Action)
drop_action = stateLinkedAction "player-turn" "drop"

wield_action :: (String,Action)
wield_action = stateLinkedAction "player-turn" "wield"

unwield_action :: (String,Action)
unwield_action = stateLinkedAction "player-turn" "unwield"

selectRaceAction :: String -> (String,Action)
selectRaceAction s = 
    (s,selectTableAction ("player-races","0","name") "race-selection" "select-race" s)

selectBaseClassAction :: String -> (String,Action)
selectBaseClassAction s = 
    (s,selectTableAction ("base-classes","0","class") "class-selection" "select-class" s)

zoomSize :: Double -> Double
zoomSize x | x < 3 = 0.2
zoomSize x | x < 10 = 1.0
zoomSize _ | otherwise = 5.0

zoomIn :: Double -> Double
zoomIn x = x - (zoomSize $ x - zoomSize x)

zoomOut :: Double -> Double
zoomOut x = x + (zoomSize x)

zoom_in_action :: (String,Action)
zoom_in_action = alwaysAction "zoom-in" $ \action_input ->
    do modifyIORef (action_globals action_input) $ \g ->
           g { global_planar_camera_distance = max 1.0 $ zoomIn $ global_planar_camera_distance g }
       return ()

zoom_out_action :: (String,Action)
zoom_out_action = alwaysAction "zoom-out" $ \action_input ->
    do modifyIORef (action_globals action_input) $ \g ->
           g { global_planar_camera_distance = min 25.0 $ zoomOut $ global_planar_camera_distance g }
       return ()

{----------------------------------------------------------
    Lists of Known Actions
 ----------------------------------------------------------}

select_race_action_names :: [String]
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

select_race_actions :: [(String,Action)]
select_race_actions = map selectRaceAction select_race_action_names

select_base_class_action_names :: [String]
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

select_base_class_actions :: [(String,Action)]
select_base_class_actions = map selectBaseClassAction select_base_class_action_names

eight_directions :: [String]
eight_directions = ["n","ne","nw","e","w","se","sw","s"]

move_actions :: [(String,Action)]
move_actions = map moveAction eight_directions

turn_actions :: [(String,Action)]
turn_actions = map turnAction eight_directions

fire_actions :: [(String,Action)]
fire_actions = map fireAction eight_directions

all_actions :: [(String,Action)]
all_actions = [continue_action,quit_action,reroll_action,pickup_action,drop_action,wield_action,unwield_action,
               zoom_in_action,zoom_out_action] ++
              select_race_actions ++ 
	      select_base_class_actions ++
	      move_actions ++
              turn_actions ++
	      fire_actions

lookupAction :: String -> (String,Action)
lookupAction x = (x,fromMaybe (error $ "tried to operate on an unknown action named " ++ x) $ lookup x all_actions)

-- |
-- Answer a complete list of all actions that can be run at this time.
-- Accepts an optional list of action names to choose from; if Nothing,
-- uses all concievable actions as that list.
--
getValidActions :: ActionInput -> Maybe [String] -> IO [String]
getValidActions action_input actions_list = 
    do valid_action_pairs <- filterM (actionValid action_input . snd) $ 
                                 maybe all_actions (map lookupAction) actions_list
       return $ map fst valid_action_pairs

-- |
-- Takes a list of action names and executes the first action in the list that is valid
-- in the current context, based on each action's action_valid entry.
-- Returns True if an action was taken, False otherwise.
--
takeUserInputAction :: ActionInput -> [String] -> IO Bool
takeUserInputAction _ [] = return False
takeUserInputAction action_input action_names =
    do valid_actions <- getValidActions action_input (Just action_names)
       let action = map lookupAction valid_actions
       case length action of
           0 -> return False
	   1 -> do executeAction action_input $ snd $ head action
                   return True
	   _ -> do hPutStrLn stderr ("Action bindings warning: multiple valid action for binding: " ++ (concat $ intersperse ", " $ map fst action))
		   executeAction action_input $ snd $ head action
                   return True
\end{code}
