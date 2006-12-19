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

module Keymaps
    (filterKeySequence,
     keysToActionNames,
     actionNameToKeys)
    where

import Actions
import Data.IORef
import Data.List
import Globals

validKeyMap :: IORef RoguestarGlobals -> IO [(String,String)]
validKeyMap globals_ref =
    do valid_actions <- getValidActions globals_ref
       globals <- readIORef globals_ref
       return $ filter (\x -> snd x `elem` valid_actions) $ global_keymap globals

-- |
-- Implements features for typing in commands: including smart typeahead completion,
-- and clearing the key sequence if it isn't valid.
--
filterKeySequence :: IORef RoguestarGlobals -> String -> IO String
filterKeySequence _ key_sequence | (length $ words key_sequence) == 0 = return ""
filterKeySequence globals_ref key_sequence =
    do valid_key_map <- validKeyMap globals_ref
       let possible_completions = filter (\x -> elem key_sequence $ inits x) $ map fst $ valid_key_map
       return $ case length possible_completions of
						 0 -> ""
						 1 -> if key_sequence /= head possible_completions
						      then init $ head possible_completions
						      else key_sequence
						 _ -> maximumBy (\x -> \y -> compare (length x) (length y)) $ foldr1 intersect $ map inits possible_completions

-- |
-- Answers the names of actions that could be executed given the specified user input.
--
keysToActionNames :: IORef RoguestarGlobals -> String -> IO [String]
keysToActionNames globals_ref key_sequence = 
    do valid_key_map <- validKeyMap globals_ref
       return $ map snd $ filter (\x -> fst x == key_sequence) $ valid_key_map

-- |
-- Reverse lookup -- get keymap entries from action names.
--
actionNameToKeys :: IORef RoguestarGlobals -> String -> IO [String]
actionNameToKeys globals_ref action_name = 
    do valid_key_map <- validKeyMap globals_ref
       return $ map fst $ filter (\x -> snd x == action_name) $ valid_key_map